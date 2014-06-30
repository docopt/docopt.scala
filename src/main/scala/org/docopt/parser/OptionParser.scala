package org.docopt.parser

import org.docopt._

import scala.util.matching.Regex.Match

object OptionParser extends CommonParser {

  // we need space matching
  override def skipWhitespace = false

  def parse(doc: String) = """\n[\t ]*(-\S+[^\n]*)""".r.findAllIn(doc).matchData flatMap {
    case Match(s) => parseSingle(s)
  }

  def parseSingle(line: String) = parseScoped(optionDefinition, line)

  protected[this] def optionDefinition = preOption ~ optionDescription ^^ {
    case e ~ v => collectDefaultValue(e, v)
  }

  private[this] def collectDefaultValue(expr: Expr,
                                        value: scala.Option[String]): Expr = (expr, value) match {
    case (Option(name, None, Some(Argument(a, _))), Some(v)) => Option(name, Argument(a, v))
    case (c:ComposedExpr, s) => c map { collectDefaultValue(_, s) }
    case (e, _) => e
  }

  // "(?s)" is used to match multiline
  protected[this] def optionDescription = """(?s)  .*""".r ^^ {
    val defaultPattern = """\[(?i)default: (.*)\]""".r
    defaultPattern.findFirstMatchIn(_) match {
      case Some(defaultPattern(v)) => Some(v)
      case _ => None
    }
  }

  /*
   * Accepts the following:
   *     "-o<arg>, --opt=<arg>" | "--opt=<arg>, -o<arg>" |
   *     "-o, --opt"            | "--opt, -o"            |
   *     "-o"                   | "-o<arg>"              |
   *     "--opt"                | "--opt=<arg>"
   */
  protected[this] def preOption = pairedOption(shortOptionNoArg, longOptionNoArg) |
                                  pairedOption(shortOptionWithArg, longOptionWithArg) |
                                  longOption | shortOption

  private[this] def pairedOption(a: Parser[Expr], b: Parser[Expr]) =
    (a ~ ( """,? """.r ~> b) ^^ { case l ~ r => Either(l, r) }) |
    (b ~ ( """,? """.r ~> a) ^^ { case l ~ r => Either(l, r) })

}
