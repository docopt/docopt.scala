package org.docopt.patterns

class Value
case class IntValue(value: Int = 0) extends Value
case class DoubleValue(value: Double = 0.0) extends Value
case class BooleanValue(value: Boolean = false) extends Value
case class StringValue(value: String = "") extends Value

trait PatternParser {
  def createFromString(patternStr: String): scala.Option[Pattern]

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
}

object PatternMatcher {
  type SPat = Seq[Pattern]
  def matchPattern(matcher: Pattern, left: SPat, collected: SPat = Nil):
    scala.Option[Tuple2[SPat, SPat]] = matcher match {
      case child:ChildPattern => matchChildPattern(child, left, collected)
      case parent:ParentPattern => matchParentPattern(parent, left, collected)
      // this should never happen
      case _ => None
    }

  private def matchParentPattern(
      matcher: ParentPattern,
      left: SPat,
      collected: SPat): scala.Option[Tuple2[SPat, SPat]] =
    Some(left, collected)

  private def matchChildPattern(
      matcher: ChildPattern,
      left: SPat,
      collected: SPat): scala.Option[Tuple2[SPat, SPat]] =
    matcher.singleMatch(left) match {
      case Some((pos, matched)) => {
        val left_ = left.slice(0, pos) ++ left.slice(pos+1, left.length)
        // TODO(fsaintjacques) fix this
        //val sameName = (for (a <- collected) yield a).toList

        Some((left_, collected ++ List(matched)))
      }
      case _ => None
  }
}

trait ChildPattern {
  def singleMatch(left: Seq[Pattern], index: Int = 0):
    scala.Option[Tuple2[Int, Pattern]]
}

trait ParentPattern {
  def children(): Seq[Pattern]
}


// Basic Argument.
abstract class Pattern
case class Argument(name: String, value: Value = StringValue()) extends Pattern with ChildPattern {
  def singleMatch(left: Seq[Pattern], index: Int = 0):
      scala.Option[Tuple2[Int, Pattern]] = left match {
    case Nil => None
    case head :: tail => head match {
      case Argument(n, v) => Some(index, Argument(name, v))
      case _ => singleMatch(tail, index+1)
    }
  }
}

case class Command(name: String, value: Value = BooleanValue(false)) extends Pattern with ChildPattern {
  def singleMatch(left: Seq[Pattern], index: Int = 0):
      scala.Option[Tuple2[Int, Pattern]] = left match {
    case Nil => None
    case head :: tail => head match {
      case Argument(n, StringValue(v)) if v == name =>
        Some(index, Command(name, BooleanValue(true)))
      case a:Argument => None
      case _ => singleMatch(tail, index+1)
    }
  }
}

case class CmdOption(
    short: String,
    long: String,
    argCount: Int = 0,
    value: Value = StringValue()) extends Pattern with ChildPattern {
  def singleMatch(left: Seq[Pattern], index: Int = 0):
      scala.Option[Tuple2[Int, Pattern]] = left match {
    case Nil => None
    case head :: tail => head match {
      case o:CmdOption if o.name == name => Some(index, o)
      case _ => singleMatch(tail, index+1)
    }
  }

  def name: String = long match {
    case "" => short
    case _ => long
  }
}

// object
object Argument extends PatternParser {
  def createFromString(source: String) : scala.Option[Argument] = {
    val name = """(<\S*?>)""".r.findFirstIn(source).getOrElse("")
    val defaultPattern = """\[(?i)default: (.*)\]""".r
    val value = defaultPattern.findFirstMatchIn(source) match {
      case Some(defaultPattern(v)) => parseValue(v)
      case None => StringValue()
    }

    Some(Argument(name, value))
  }
}

object CmdOption extends PatternParser {
  def createFromString(optionDescription: String) : scala.Option[CmdOption] = {
    // this replace removes traling whitespaces
    optionDescription.replaceAll("""(?m)\s+$""", "").split("  ").filter(_ != "") match {
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
        val defaultPattern = """\[(?i)default: (.*)\]""".r
        val value = defaultPattern.findFirstMatchIn(description) match {
          case Some(defaultPattern(v)) => parseValue(v)
          case _ => StringValue()
        }
        Some(CmdOption(short, long, argcount, value))
      }

      case _ => None
    }
  }

  def parseDefaults(doc: String): Iterator[CmdOption] = {
    val optionMatchRegex = """\n[\t ]*(-\S+[^\n]*)""".r
    for (optionMatch <- optionMatchRegex.findAllIn(doc).matchData;
         option <- CmdOption.createFromString(optionMatch.group(1))) yield option
  }
}
