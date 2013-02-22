import scala.util.matching.Regex;
import scala.collection.immutable.WrappedString;

abstract class Pattern(val name: String, val value: String) {
  // force the implementors to define toString
  def toString: String

  override def equals(that: Any) = this.toString == that.toString
}

abstract class ChildPattern(name: String, value: String) extends Pattern(name, value) {
  override def toString : String = "%s(%s, %s)".format(cleanName, name, value)
  val cleanName = "ChildPattern"

  def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int,Pattern]]

  // def flat(types: Set[AnyRef]) =
    // if (this.) List(this) else Nil

  // def match(left: String, collected: Set[AnyRef]) {
    // val posMatch = this.singleMatch
  // }
}

class Argument(name: String, value: String) extends ChildPattern(name, value) {
  override val cleanName = "Argument"
  def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int, Pattern]] = {
    for ((arg, i) <- left.zipWithIndex)
      if (arg.isInstanceOf[Argument])
        Some(Tuple2(i, new Argument(this.name, arg.value)))
    None
  }
}

object Argument {
  def createFromString(source: String) : Argument = {
    val name = """(<\S*?>)""".r.findFirstIn(source).getOrElse("")
    val defaultPattern = """\[default: (.*)\]""".r
    val value = defaultPattern.findFirstMatchIn(source) match {
      case Some(defaultPattern(v)) => v
      case None => ""
    }

    new Argument(name, value)
  }
}

class Command(name: String, value: String) extends Argument(name, value) {
  override val cleanName = "Command"
  override def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int, Pattern]] =  {
    for ((arg, i) <- left.zipWithIndex)
      if (arg.isInstanceOf[Argument])
        if (arg.value == this.name)
          Some(Tuple2(i, new Command(this.name, "true")))
        else
          None
    None
  }
}

class CmdOption(short: String = "", long: String = "", argCount: Int, value: String) extends ChildPattern(long ++ short, value) {
  override def toString : String = "CmdOption(%s, %s, %s, %s)".format(short, long, argCount, value)
  override def singleMatch(left: List[Pattern]) : scala.Option[Tuple2[Int, Pattern]] =  {
    for ((arg, i) <- left.zipWithIndex)
      if (arg.isInstanceOf[Argument])
        // voir si name == long++short détruit la logique
        if (arg.value == this.name)
          Some(Tuple2(i, new Command(this.name, "true")))
        else
          None
    None
  }
}

object CmdOption {
  def createFromString(optionDescription: String) : scala.Option[CmdOption] = {
    // this replace removes traling whitespaces
    optionDescription.replaceAll("""(?m)\s+$""", "").split("  ").filter(_ != "") match {
      case Array(options, description) => {
        val options_ = options.replace(",", " ").replace("=", " ").split(" ").filter(_ != "")
        var short, long, value = ""
        var argcount = 0
        // sadly docopt is not strict on ordering, the easiest way to do
        // it is with a for loop.
        for (s <- options_) {
          if (s.startsWith("--")) long = s
          else if (s.startsWith("-")) short = s
          else argcount = 1
        }
        if (argcount > 0) {
          val defaultPattern = """\[default: (.*)\]""".r
          value = defaultPattern.findFirstMatchIn(description) match {
            case Some(defaultPattern(v)) => v
            case _ => ""
          }
        }
        Some(new CmdOption(short, long, argcount, value))
      }

      case _ => None
    }
  }
}

// def parseDefaults(doc: String): Iterator[CmdOption] = {
  // val optionMatchRegex = """\n[\t ]*(-\S+[^\n]*)""".r
  // for (optionMatch <- optionMatchRegex.findAllIn(doc).matchData;
       // option <- CmdOption.createFromString(optionMatch.group(1))) yield option
// }
