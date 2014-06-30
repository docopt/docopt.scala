package org.docopt.parser

import org.docopt.{Argument, Command, Expr, Option}

import scala.util.parsing.combinator.RegexParsers

trait CommonParser extends RegexParsers {

  protected[this] def parseScoped(parser: Parser[Expr], input: String) = {
    parse[Expr](parser, input) match {
      case Success(res, reader) if reader.atEnd => Some(res)
      case _ => None
    }
  }

  protected[this] def longOption = longOptionWithArg | longOptionNoArg

  protected[this] def shortOption = shortOptionWithArg | shortOptionNoArg

  private[this] def optionWithArg(option: Parser[Option], argDef: Parser[Argument]) =
    option ~ argDef ^^ { case l ~ a => Option(l.name, a ) }

  protected[this] def longOptionWithArg = optionWithArg(longOptionNoArg, "=" ~> argument)

  protected[this] def shortOptionWithArg = optionWithArg(shortOptionNoArg, argument)

  protected[this] def longOptionNoArg: Parser[Option] = """--[a-zA-Z0-9-]+""".r  ^^ { Option (_) }

  protected[this] def manyShortOptions: Parser[Option] = """-[a-zA-Z0-9]+""".r ^^ { Option(_) }

  protected[this] def shortOptionNoArg: Parser[Option] = """-[a-zA-Z0-9]""".r ^^ { Option(_) }

  protected[this] def argument = '<' ~> """[a-zA-Z0-9- ]+""".r <~ '>' ^^ { Argument(_) }

  protected[this] def command = """[a-z-]+""".r ^^ { Command(_) }

  protected[this] def spacing = """[ \t]*""".r

}
