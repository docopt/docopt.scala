package org.docopt

import scala.util.matching.Regex;
import scala.collection.immutable.WrappedString;

trait PatternStringParser {
  def createFromString(patternString: String) : scala.Option[Pattern]
}

abstract class Pattern {
  // force the implementors to define toString
  def toString: String

  // Flaten Tree
  def flat(typeName: String): List[Pattern]

  //
  def matchPattern(left: List[Pattern], collected: List[Pattern] = Nil):
      scala.Option[Tuple2[List[Pattern],List[Pattern]]]

  override def equals(that: Any) = this.toString == that.toString
}

abstract class ParentPattern(val children: List[Pattern]) extends Pattern {
  val cleanName = "ParentPattern"
  override def toString =
    "%s(%s)".format(cleanName, children.map(_.toString).mkString(","))

  def flat(typeName: String) =
   if (typeName != "" && this.cleanName == typeName) List(this)
   else (for (c <- children) yield c.flat(typeName)).toList.flatten
}

class Required(children: List[Pattern]) extends ParentPattern(children) {
  def matchPattern(left: List[Pattern], collected: List[Pattern] = Nil):
      scala.Option[Tuple2[List[Pattern],List[Pattern]]] =
    None

  // private def matchPatternTail(
      // l: List[Pattern],
      // c:List[Pattern],
      // childs: List[Pattern]) = {
    // if (childs == Nil) Some(l,c)
    // else childs.head.matchPattern(l,c) match {
      // case None => None
      // case Some(l_,c_) => matchPatternTail(l_, c_, childs.tail)
    // }
  // }

}

abstract class ChildPattern(val name: String, val value: String) extends Pattern {
  val cleanName = "ChildPattern"
  override def toString = "%s(%s, %s)".format(cleanName, name, value)

  def flat(typeName: String = "") : List[Pattern] =
    if (typeName != "" && this.cleanName == typeName) List(this) else Nil

  def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int,Pattern]]

  def matchPattern(left: List[Pattern], collected: List[Pattern] = Nil) : scala.Option[Tuple2[List[Pattern],List[Pattern]]] = {
    val maybeMatch = singleMatch(left)
    maybeMatch match {
      case Some((position, matched)) => {
        val left_ = left.slice(0, position) ++ left.slice(position+1, left.length)
        val sameName = (for (a <- collected; if (a.isInstanceOf[ChildPattern] && a.asInstanceOf[ChildPattern].name == this.name)) yield a).toList
        // TODO(fsaintjacques) finish this.
        Some(left_, collected ++ List(matched))
      }
      case _ => None
    }
  }
}

class Argument(name: String, value: String) extends ChildPattern(name, value) {
  override val cleanName = "Argument"
  def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int, Pattern]] = {
    for ((arg, i) <- left.zipWithIndex)
      if (arg.isInstanceOf[Argument])
        return Some(i, new Argument(this.name, arg.asInstanceOf[Argument].value))
    None
  }
}

object Argument extends PatternStringParser {
  def createFromString(source: String) : scala.Option[Argument] = {
    val name = """(<\S*?>)""".r.findFirstIn(source).getOrElse("")
    val defaultPattern = """\[(?i)default: (.*)\]""".r

    // TODO(fsaintjacques): we can probably shorten this
    val value = defaultPattern.findFirstMatchIn(source) match {
      case Some(defaultPattern(v)) => v
      case None => ""
    }

    Some(new Argument(name, value))
  }
}

class Command(name: String, value: String) extends Argument(name, value) {
  override val cleanName = "Command"
  override def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int, Pattern]] =  {
    for ((arg, i) <- left.zipWithIndex)
      if (arg.isInstanceOf[Argument]) {
        if (arg.asInstanceOf[Argument].value == this.name) {
          return Some(i, new Command(this.name, "true"))
        }
        else
          return None
      }
    None
  }
}

class CmdOption(val short: String, val long: String, val argCount: Int = 0, value: String = "") extends ChildPattern(if (long != "") long else short, value) {
  override def toString : String =
    "CmdOption(%s, %s, %s, %s)".format(short, long, argCount, value)

  override def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int, Pattern]] = {
    for ((arg, i) <- left.zipWithIndex) {
      if (arg.isInstanceOf[CmdOption] &&
          arg.asInstanceOf[CmdOption].name == this.name)
        return Some((i, arg))
    }
    None
  }
}

object CmdOption extends PatternStringParser {
  def createFromString(optionDescription: String) : scala.Option[CmdOption] = {
    // this replace removes traling whitespaces
    optionDescription.replaceAll("""(?m)\s+$""", "").split("  ").filter(_ != "") match {
      case Array(options, description) => {
        val options_ = options.replace(",", " ").replace("=", " ").split(" ").filter(_ != "")
        // extract short, long, argcount from options
        val (short, long, argcount) = options_.foldLeft(("","",0)) {
          case (tuple, token) => {
            token match {
              case token if token startsWith "--" => (tuple._1, token, tuple._3) // long option
              case token if token startsWith "-" => (token, tuple._2, tuple._3) // short option
              case _ => (tuple._1, tuple._2, 1) // unamed argument
            }
          }
        }
        val defaultPattern = """\[(?i)default: (.*)\]""".r
        val value = defaultPattern.findFirstMatchIn(description) match {
          case Some(defaultPattern(v)) => v
          case _ => ""
        }
        Some(new CmdOption(short, long, argcount, value))
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
