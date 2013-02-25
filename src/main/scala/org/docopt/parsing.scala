package org.docopt.parsing

import scala.{Option => SOption}
import org.docopt.patterns._

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
      case Some(defaultPattern(v)) => PatternParser.parseValue(v)
      case None => StringValue()
    }
  }

  def parseArgument(argument: String) : SOption[Argument] =
    Some(Argument("""(<\S*?>)""".r.findFirstIn(argument).getOrElse(""),
                  parseDefault(argument)))

  def parseOption(option: String) : SOption[Option] = {
    // this replace removes traling whitespaces
    option.replaceAll("""(?m)\s+$""", "").split("  ").filter(_ != "") match {
      case Array(options, description) => {
        val options_ = options.replace(",", " ").replace("=", " ").split(" ").filter(_ != "")
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
  }

  def parseOptionsDescriptions(doc: String): Iterator[Option] = {
    val optionMatchRegex = """\n[\t ]*(-\S+[^\n]*)""".r
    for (optionMatch <- optionMatchRegex.findAllIn(doc).matchData;
         option <- parseOption(optionMatch.group(1))) yield option
  }
}
