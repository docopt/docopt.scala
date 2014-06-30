package org.docopt.parser

import org.docopt._

object ArgvParser extends CommonParser {

  def parse(argv: List[String]): Expr = {
    val (withOptions, noOptions) = argv.span(_ != "--")
    // note that exprs should only contain Expr in {Command, Option}
    val exprs = withOptions map { parseScoped(arg, _) } map { case Some(e)  => e }
    val left = noOptions map { Command(_) }
    Required(exprs ++ left)
  }

  protected[this] def arg = argvLongOption | argvShortOption | command

  protected[this] def argvLongOption =
    """--[a-zA-Z0-9-]+""".r ~ opt('=' ~> """.+""".r) ^^ {
      case l ~ v => Option(l, v map { Value(_) } )
    }

  protected[this] def argvShortOption =
    """-[a-zA-Z0-9]""".r ~ opt(""".+""".r) ^^ {
      case o ~ v => Option(o, v map { Value(_) } )
    }
}
