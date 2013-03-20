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
  // TODO(fsaintjacques): clean up this
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

  def fixPattern(pattern: Pattern): Pattern =
    fixRepeatingArgument(pattern)

  def fixRepeatingArgument(pattern: Pattern, uniq: List[Pattern] = Nil): Pattern =
    pattern

  def eitherPattern(pattern: Pattern): Pattern =
    pattern

  def stringStrip(str: String): String =
    str.replaceAll("""(?m)\s+$""", "").replaceAll("""^\s+(?m)""", "")
}
