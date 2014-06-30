package org.docopt.parser

import org.docopt._

object UsageParser extends CommonParser {

  def parse(input: String): scala.Option[Expr] = parseScoped(usageBlock, input)

  def parseSingle(input: String): scala.Option[Expr] = parseScoped(usage, input)

  protected[this] def usageBlock = rep1sep(usage, "\n") ^^ { Either(_) }

  protected[this] def usage = rep1(expr) ^^ { Required(_)}

  protected[this] def expr: Parser[Expr] = spacing ~> basicExpr ~ opt(oneOrMore) <~ spacing ^^ {
    case e ~ Some(_) => OneOrMore(List(e))
    case e ~ None => e
  }

  protected[this] def oneOrMore = "..." ^^ { _ => OneOrMore(Nil) }

  protected[this] def basicExpr: Parser[Expr] = anyOptions | required    | optional         |
                                                longOption | shortOption | manyShortOptions |
                                                argument   | command

  protected[this] def required = '(' ~> repsep(expr, "|") <~ ')' ^^ { Required(_) }

  protected[this] def optional = '[' ~> repsep(expr, "|") <~ ']' ^^ { Optional(_) }

  protected[this] def anyOptions = "[options]" ^^ { _ => AnyOptions(Nil) }
}

