package org.docopt.parser

import org.docopt.parser.OptionParser._
import org.docopt.{Argument, Either, Option}
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class OptionParserTest extends FunSpec with ShouldMatchers {

  describe("OptionParser should parse single options") {

    val shortOpt = "-o  Some random text"
    it("should parse '" + shortOpt + "'") {
      parseSingle(shortOpt) should be (Some(Option("-o")))
    }

    val shortOptArg = "-o<arg>  Some random text"
    it("should parse '" + shortOptArg + "'") {
      parseSingle(shortOptArg) should be (Some(Option("-o", Argument("arg"))))
    }

    val longOpt = "--opt  Some random text."
    it("should parse '" + longOpt + "'") {
      parseSingle(longOpt) should be (Some(Option("--opt")))
    }

    val longOptArg = "--opt=<arg>  Some random text."
    it("should parse '" + longOptArg + "'") {
      parseSingle(longOptArg) should be (Some(Option("--opt", Argument("arg"))))
    }

    val shortLongOpt = "-o, --opt  Some random text."
    it("should parse '" + shortLongOpt + "'") {
      parseSingle(shortLongOpt) should be (Some(Either(Option("-o"),Option("--opt"))))
    }

    val shortLongOptArg = "-o<arg>, --opt=<arg>  Some random text."
    it("should parse '" + shortLongOptArg + "'") {
      parseSingle(shortLongOptArg) should be (Some(Either(Option("-o", Argument("arg")),
                                                          Option("--opt", Argument("arg")))))
    }

    val longDefault = "--opt=<arg>  Some random text [default: myValue]"
    it("should parse '" + longDefault + "'") {
      parseSingle(longDefault) should be (Some(Option("--opt", Argument("arg", "myValue"))))
    }

    val complexDefault = "-o<arg>, --opt=<arg>  Some random text [default: 3]"
    it("should parse '" + complexDefault + "'") {
      parseSingle(complexDefault) should be (Some(Either(Option("-o", Argument("arg", "3")),
                                                         Option("--opt", Argument("arg", "3")))))
    }

    val multilineDefault =
      """|-m<arg>, --mul=<arg>  Some multiline text
         |                        moreText [default: m]""".stripMargin
    it("should parse multiline options definition'") {
      parseSingle(multilineDefault) should be (Some(Either(Option("-m", Argument("arg", "m")),
                                                           Option("--mul", Argument("arg", "m")))))
    }

    val missingSpace = "-oSome random text"
    it("should require 2 spaces '" + missingSpace + "'") {
      parseSingle(missingSpace) should be (None)
    }
  }

}
