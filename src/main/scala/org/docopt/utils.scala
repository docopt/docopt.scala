package org.docopt

import org.docopt.pattern._

package object utils {
  // type shortcut used in many places
  type SeqPat = Seq[Pattern]

  val UnconsumedTokensMessage = """Some tokens were not consumed by the pattern parser: '[%s]'"""
  val MissingEnclosureMessage = """Missing enclosure character '%s'."""
  val UnparsableOptionMessage = """Could not parse option '%s' correctly."""
  val UnexpectedArgumentMessage = """Found argument '%s' but not expected."""
  val MissingArgumentMessage = """Expected argument not found in '%s'."""
  val UnmatchedUsageMessage = """Not matched."""

  class DocoptException(msg: String) extends RuntimeException(msg)
  class DocoptExitException(msg: String) extends DocoptException(msg)
  class DocoptLanguageException(msg: String) extends DocoptException(msg)
  class UnconsumedTokensException(tokens: Seq[String])
    extends DocoptLanguageException(UnconsumedTokensMessage.format(tokens.mkString(",")))
  class MissingEnclosureException(enclosure: String)
    extends DocoptLanguageException(MissingEnclosureMessage.format(enclosure))
  class UnparsableOptionException(optionToken: String)
    extends DocoptLanguageException(UnparsableOptionMessage.format(optionToken))
  class UnexpectedArgumentException(token: String)
    extends DocoptLanguageException(UnexpectedArgumentMessage.format(token))
  class MissingArgumentException(token: String)
    extends DocoptLanguageException(MissingArgumentMessage.format(token))
  class UnmatchedUsageException(token: String, usage: String)
    extends DocoptExitException(UnmatchedUsageMessage)

  def samePattern(first: Pattern)(second: Pattern): Boolean =
    (first, second) match {
      case (a:Argument, b:Argument) => true
      case (a:Command, b:Command) => true
      case (a:Option, b:Option) => true
      case (a:Required, b:Required) => true
      case (a:Optional, b:Optional) => true
      case (a:AnyOptions, b:AnyOptions) => true
      case (a:OneOrMore, b:OneOrMore) => true
      case (a:Either, b:Either) => true
      case _ => false
    }

  def patternTypeIn(pattern: Pattern, types: SeqPat): Boolean =
    types exists samePattern(pattern)

  def flattenPattern(pattern: Pattern,
                     specific: SeqPat = Nil): SeqPat =
    pattern match {
      case child:ChildPattern => specific match {
        case Nil => List(pattern)
        case head :: tail if patternTypeIn(child, specific) => List(pattern)
        case _ => Nil }
      case p:ParentPattern => patternTypeIn(pattern, specific) match {
        case true => List(pattern)
        case false => p.children.flatMap { flattenPattern(_, specific) } }
    }

  def fixPattern(pattern: Pattern, options: List[Pattern] = Nil): Pattern =
    fixRepeatingArgument(
      fixAnyOptions(pattern,
                    (Set(options:_*) -- Set(flattenPattern(pattern, List(Option("",""))):_*)).toList))

  def fixAnyOptions(pattern: Pattern, options: List[Pattern]): Pattern = pattern match {
    case a:AnyOptions => AnyOptions(options:_*)
    case r:Required => Required(r.children.map(fixAnyOptions(_, options)))
    case e:Either => Either(e.children.map(fixAnyOptions(_, options)))
    case o:Optional => Optional(o.children.map(fixAnyOptions(_, options)))
    case o:OneOrMore => OneOrMore(o.children.map(fixAnyOptions(_, options)))
    case other => other
  }

  def fixRepeatingArgument(pattern: Pattern): Pattern = {
    val eithers = eitherPattern(pattern).children.filter(
      { case r:Required => true
        case _ => false}).map({case r:Required => r.children})
    fixRepeatingArgumentRec(pattern, repeatedArguments(eithers))
  }

  def repeatedArguments(childs: List[List[Pattern]]): Set[String] =
    childs.map(x => Set(x.filter({
      case c:ChildPattern => true
      case _ => false})
      .groupBy({case c:ChildPattern => c.name})
      .filter({case (name, group) => group.length > 1 case _ => false})
      .map({case (name, group) => name}).toList:_*)).foldLeft(Set[String]())((a,b) => a ++ b)

  private def fixRepeatingArgumentRec(pattern: Pattern, repeated: Set[String]): Pattern = pattern match {
    case r:Required => Required(r.children.map(x => fixRepeatingArgumentRec(x, repeated)))
    case o:Optional => Optional(o.children.map(x => fixRepeatingArgumentRec(x, repeated)))
    case a:AnyOptions => AnyOptions(a.children.map(x => fixRepeatingArgumentRec(x, repeated)))
    case o:OneOrMore => OneOrMore(o.children.map(x => fixRepeatingArgumentRec(x, repeated)))
    case e:Either => Either(e.children.map(x => fixRepeatingArgumentRec(x, repeated)))
    case Argument(n, NullValue(_)) if repeated.contains(n) => Argument(n, ManyStringValue())
    case Argument(n, StringValue(v)) if repeated.contains(n) => Argument(n, ManyStringValue(v.split(" ").toList))
    case o@Option(s, l, c, NullValue(_)) if c > 0 && repeated.contains(o.name) => Option(s, l, c, ManyStringValue())
    case o@Option(s, l, c, StringValue(v)) if c > 0 && repeated.contains(o.name) => Option(s, l, c, ManyStringValue(v.split(" ").toList))
    case o@Option(s, l, c, _) if c == 0 && repeated.contains(o.name) => Option(s, l, c, IntValue(0))
    case Command(n, _) if repeated.contains(n) => Command(n, IntValue(0))
    case other => other
  }

  def eitherPattern(pattern: Pattern): Either =
    eitherPatternRec(List(List(pattern)), Nil)

  private def eitherPatternRec(groups: List[List[Pattern]], ret: List[List[Pattern]]): Either = groups match {
    case Nil => Either(ret.map(children => Required(children:_*)):_*)
    case children :: groups_ => {
      if (children exists samePattern(Either(Nil))) {
        val (either:Either, children_) = removeFirst(children)(samePattern(Either(Nil)))
        eitherPatternRec(groups_ ::: either.children.map(c => List(c) ++ children_), ret)
      } else if (children exists samePattern(Required(Nil))) {
        val (required:Required, children_) = removeFirst(children)(samePattern(Required(Nil)))
        eitherPatternRec(groups_ ::: List(required.children ::: children_), ret)
      } else if (children exists samePattern(Optional(Nil))) {
        val (optional:Optional, children_) = removeFirst(children)(samePattern(Optional(Nil)))
        eitherPatternRec(groups_ ::: List(optional.children ::: children_), ret)
      } else if (children exists samePattern(AnyOptions(Nil))) {
        val (optional:AnyOptions, children_) = removeFirst(children)(samePattern(AnyOptions(Nil)))
        eitherPatternRec(groups_ ::: List(optional.children ::: children_), ret)
      } else if (children exists samePattern(OneOrMore(Nil))) {
        val (more:OneOrMore, children_) = removeFirst(children)(samePattern(OneOrMore(Nil)))
        eitherPatternRec(groups_ ::: List(more.children ::: more.children ::: children_), ret)
      } else {
        // No more ParentPattern
        eitherPatternRec(groups_, ret ++ List(children))
      }
    }
  }

  def removeFirst[T](list: List[T])(predicate: T => Boolean): (T, List[T]) = {
    val (before, elem :: after) = list span (x => !predicate(x))
    (elem, before ::: after)
  }

  def remove(num: Int, list: List[Any]) = list diff List(num)

  def stringStrip(str: String): String =
    str.replaceAll("""(?m)\s+$""", "").replaceAll("""^\s+(?m)""", "")
}
