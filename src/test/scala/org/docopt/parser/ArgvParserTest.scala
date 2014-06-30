package org.docopt.parser

import org.docopt.parser.ArgvParser._
import org.docopt.{Command, Option, Required, Value}
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ArgvParserTest extends FunSpec with ShouldMatchers {

  describe("ArgvParser") {
    it("should parse 'a b c -v --help'") {
      parse(List("a","b","c","-v","--help")) should be (
        Required(Command("a"),
                 Command("b"),
                 Command("c"),
                 Option("-v"),
                 Option("--help")))
    }

    it("should parse 'a-b -oarg1 --help=arg2'") {
      parse(List("a-b","-oarg1","--help=arg2")) should be (
        Required(Command("a-b"),
                 Option("-o", Value("arg1")),
                 Option("--help", Value("arg2"))))
    }

    it("should parse '-- -'") {
      parse(List("--","-")) should be (Required(Command("--"), Command("-")))
    }
  }

}
