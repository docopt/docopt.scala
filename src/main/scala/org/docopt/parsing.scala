package org.docopt.parsing

import scala.util.matching.{Regex}
import scala.util.matching.Regex.{Match}
import scala.collection.immutable.{Queue}
import scala.{Option => SOption}
import org.docopt.pattern._

object PatternParser {
  def parseValue(value: String): Value = {
    val intPattern = """^[0-9]+$""".r
    val doublePattern = """^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$""".r
    val booleanPattern = """^(?i)(true|false)$""".r

    if (intPattern.findFirstIn(value).isDefined)
      IntValue(value.toInt)
    else if (doublePattern.findFirstIn(value).isDefined)
      DoubleValue(value.toDouble)
    else if (booleanPattern.findFirstIn(value).isDefined)
      BooleanValue(value.toBoolean)
    else
      StringValue(value)
  }

  def parseDefault(default: String): Value = {
    val defaultPattern = """\[(?i)default: (.*)\]""".r
    defaultPattern.findFirstMatchIn(default) match {
      case Some(defaultPattern(v)) => StringValue(v)
      case None => StringValue()
    }
  }

  def parseArgument(argument: String): SOption[Argument] =
    """(<\S*?>)""".r.findFirstIn(argument).map(name =>
      Argument(name, parseDefault(argument)))

  def parseOption(option: String): SOption[Option] =
    // this replace removes traling whitespaces
    option.replaceAll("""(?m)\s+$""", "").split("  ").filter(_ != "") match {
      case Array(options, description) => {
        val options_ = options.replace(",", " ")
                              .replace("=", " ")
                              .split(" ")
                              .filter(_ != "")
        // extract short, long, argcount from options
        val (short, long, argcount) = options_.foldLeft(("","",0)) {
          case (tup, tok) => {
            tok match {
              case tok if tok startsWith "--" => (tup._1, tok, tup._3) // long option
              case tok if tok startsWith "-" => (tok, tup._2, tup._3) // short option
              case _ => (tup._1, tup._2, 1) // unamed argument
            }
          }
        }
        Some(Option(short, long, argcount, parseDefault(description)))
      }
      case _ => None
    }

  def parseOptionDescriptions(doc: String): Iterator[Option] =
    for (optionMatch <- """\n[\t ]*(-\S+[^\n]*)""".r .findAllIn(doc).matchData;
         option <- parseOption(optionMatch.group(1))) yield option

  def parsePattern(source: String, options: Seq[Option]): Pattern = {
    val tokenizeRegex = new Regex("""([\[\]\(\)\|]|\.\.\.)""", "delim")
    val tokens = tokenStream(tokenizeRegex replaceAllIn
      (source, (m: Match) => " %s".format(m.group("delim"))))
    val results = parseExpr(tokens, options)
    Required(results.toList)
  }


  def parseExpr(tokens: Seq[String], options: Seq[Option]): Seq[Pattern] = {
    val seq = parseSeq(tokens, options)
    List(AnyOptions())
  }

  def parseSeq(tokens: Seq[String], options: Seq[Option]): Seq[Pattern] =
    List(AnyOptions())

  def parseAtom(tokens: Seq[String], options: Seq[Option]): Seq[Pattern] =
    List(AnyOptions())

  def parseLongOption(tokens: Seq[String],
                      options: Seq[Option]): Seq[Option] =
    List(Option("","--long"))

  def parseShortOption(tokens: Seq[String],
                       options: Seq[Option]): Seq[Option] =
    List(Option("-s",""))

  def parseArgV(source: String, options: Seq[Option]): Seq[Pattern] =
    List(Option("-s",""))


  private def tokenStream(source: String, split: Boolean = true): Queue[String] = {
    val queue:Queue[String] = Queue()
    Queue().enqueue(source.split("\\s+").toList)
  }
}
