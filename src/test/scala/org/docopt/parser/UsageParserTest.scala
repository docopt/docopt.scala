package org.docopt.parser

import org.docopt._
import org.docopt.parser.UsageParser._
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class UsageParserTest extends FunSpec with ShouldMatchers {

  describe("Command parsing") {
    it("should parse 'command'") {
      parseSingle("command") should be (Some(Required(Command("command"))))
    }
  }

  describe("Argument parsing") {
    it("should parse '<arg>' correctly") {
      parseSingle("<arg>") should be (Some(Required(Argument("arg"))))
    }

    it("should parse '<arg 123 ->' correctly") {
      parseSingle("<arg 123 ->") should be (Some(Required(Argument("arg 123 -"))))
    }
  }

  describe("Short Option parsing") {
    it("should parse '-o'") {
      parseSingle("-o") should be (Some(Required(Option("-o", None))))
    }

    it("should parse '-o<arg>'") {
      parseSingle("-o<arg>") should be (Some(Required(Option("-o", Argument("arg")))))
    }

    it("should not parse '-o=<arg>'") {
      parseSingle("-o=<arg>") should be (None)
    }
  }

  describe("Long Option parsing") {
    it("should parse '--long'") {
      parseSingle("--long") should be (Some(Required(Option("--long", None))))
    }

    it("should parse '--long=<arg>'") {
      parseSingle("--long=<arg>") should be (Some(Required(Option("--long", Argument("arg")))))
    }
  }

  describe("AnyOption parsing") {
    it("should parse '[options]'") {
      parseSingle("[options]") should be (Some(Required(AnyOptions(Nil))))
    }
  }

  describe("Optional parsing") {
    it("should parse '[-s]'") {
      parseSingle("[-s]") should be (Some(Required(Optional(Option("-s")))))
    }

    it("should parse '[cmd|<arg>]'") {
      parseSingle("[cmd|<arg>]") should be (
        Some(Required(Optional(Command("cmd"),
                               Argument("arg")))))
    }

    it("should parse '[  cmd  |<arg>     ]'") {
      parseSingle("[  cmd  |<arg>     ]") should be (
        Some(Required(Optional(Command("cmd"),
                               Argument("arg")))))
    }
  }

  describe("Require parsing") {
    it("should parse '(-s)'") {
      parseSingle("(-s)") should be (Some(Required(Required(Option("-s")))))
    }

    it("should parse '(cmd|<arg>)'") {
      parseSingle("(cmd|<arg>)") should be (
        Some(Required(Required(Command("cmd"),
                               Argument("arg")))))
    }

    it("should parse '(  cmd  |<arg>    )'") {
      parseSingle("(  cmd  |<arg>    )") should be (
        Some(Required(Required(Command("cmd"),
                               Argument("arg")))))
    }
  }

  describe("Naval fate example") {
    it("should parse 'ship new <name>...'") {
      parseSingle("ship new <name>...") should be (
        Some(Required(Command("ship"),
                      Command("new"),
                      OneOrMore(Argument("name")))))
    }

    it("should parse 'ship <name> move<x> <y> [--speed=<kn>]'") {
      parseSingle("ship <name> move <x> <y> [--speed=<kn>]") should be (
        Some(Required(Command("ship"),
                      Argument("name"),
                      Command("move"),
                      Argument("x"),
                      Argument("y"),
                      Optional(Option("--speed", Argument("kn"))))))
    }

    it("should parse 'ship shoot <x> <y>'") {
      parseSingle("ship shoot <x> <y>") should be (
        Some(Required(Command("ship"),
                      Command("shoot"),
                      Argument("x"),
                      Argument("y"))))
    }

    it("should parse 'mine (set|remove) <x> <y> [--moored|--drifting]'") {
      parseSingle("mine (set|remove) <x> <y> [--moored|--drifting]") should be (
        Some(Required(Command("mine"),
                      Required(Command("set"),Command("remove")),
                      Argument("x"),
                      Argument("y"),
                      Optional(Option("--moored"), Option("--drifting")))))
    }
  }

}
