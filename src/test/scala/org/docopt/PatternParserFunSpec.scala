package org.docopt

import org.docopt.{PatternParser => PP}
import org.docopt.pattern._
import org.docopt.utils._
import org.scalatest.FunSpec

class PatternParserFunSpec extends FunSpec {
  describe("An Argument") {
    // Parsing
    val validArgument = bracketArgument
    it("should parse correctly: '%s'".format(validArgument)) {
      val arg = PP.parseArgument(validArgument)
      assert (arg.get == Argument(validArgument))
    }

    val ValidArgumentDefault = "%s [default: %s]".format(bracketArgument, stringValue)
    it("should parse correctly: '%s'".format(ValidArgumentDefault)) {
      val arg = PP.parseArgument(ValidArgumentDefault)
      assert (arg.get == Argument(bracketArgument, StringValue(stringValue)))
    }
  }

  describe("An Option") {
    val ValidshortOption = "  %s  %s.".format(shortOption, description)
    it("should parse correctly: '%s'".format(ValidshortOption)) {
      val opt = PP.parseOption(ValidshortOption)
      assert (opt.get == Option(shortOption, ""))
    }

    val ValidlongOption = "  %s  %s.".format(longOption, description)
    it("should parse correctly: '%s'".format(ValidlongOption)) {
      val opt = PP.parseOption(ValidlongOption)
      assert (opt.get == Option("", longOption))
    }

    val ValidShortlongOption = "  %s %s  %s.".format(shortOption, longOption, description)
    it("should parse correctly: '%s'".format(ValidShortlongOption)) {
      val opt = PP.parseOption(ValidShortlongOption)
      assert (opt.get == Option(shortOption, longOption))
    }

    val ValidShortCommalongOption = "  %s, %s  %s.".format(shortOption, longOption, description)
    it("should parse correctly: '%s'".format(ValidShortCommalongOption)) {
      val opt = PP.parseOption(ValidShortCommalongOption)
      assert (opt.get == Option(shortOption, longOption))
    }

    val ValidShortLongInverseOption = "  %s %s  %s.".format(longOption, shortOption, description)
    it("should parse correctly: '%s'".format(ValidShortLongInverseOption)) {
      val opt = PP.parseOption(ValidShortLongInverseOption)
      assert (opt.get == Option(shortOption, longOption))
    }

    val ValidShortSpaceArgumentOption = "  %s %s  %s".format(shortOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortSpaceArgumentOption)) {
      val opt = PP.parseOption(ValidShortSpaceArgumentOption)
      assert (opt.get == Option(shortOption, "", 1, NullValue()))
    }

    val ValidShortEqualArgumentOption = "  %s=%s  %s".format(shortOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortEqualArgumentOption)) {
      val opt = PP.parseOption(ValidShortEqualArgumentOption)
      assert (opt.get == Option(shortOption, "", 1, NullValue()))
    }

    val ValidLongSpaceArgumentOption = "  %s %s  %s".format(longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidLongSpaceArgumentOption)) {
      val opt = PP.parseOption(ValidLongSpaceArgumentOption)
      assert (opt.get == Option("", longOption, 1, NullValue()))
    }

    val ValidShortLongArgumentOption = "  %s %s %s %s  %s.".format(shortOption, capitalArgument, longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortLongArgumentOption)) {
      val opt = PP.parseOption(ValidShortLongArgumentOption)
      assert (opt.get == Option(shortOption, longOption, 1, NullValue()))
    }

    val ValidShortCommaLongArgumentOption = "  %s %s, %s %s  %s.".format(shortOption, capitalArgument, longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortCommaLongArgumentOption)) {
      val opt = PP.parseOption(ValidShortCommaLongArgumentOption)
      assert (opt.get == Option(shortOption, longOption, 1, NullValue()))
    }

    val ValidShortCommaLongEqualArgumentOption = "  %s %s, %s=%s  %s.".format(shortOption, capitalArgument, longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortCommaLongEqualArgumentOption)) {
      val opt = PP.parseOption(ValidShortCommaLongEqualArgumentOption)
      assert (opt.get == Option(shortOption, longOption, 1, NullValue()))
    }

    val ValidShortArgumentDefaultOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, stringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultOption)
      assert (opt.get == Option(shortOption, "", 1, StringValue(stringValue)))
    }

    val ValidShortArgumentDefaultIntOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, intValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultIntOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultIntOption)
      assert (opt.get == Option(shortOption, "", 1, StringValue(intValue)))
    }

    val ValidShortArgumentDefaultFloatOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, doubleValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultFloatOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultFloatOption)
      assert (opt.get == Option(shortOption, "", 1, StringValue(doubleValue)))
    }

    val ValidShortArgumentDefaultPathOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, pathValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultPathOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultPathOption)
      assert (opt.get == Option(shortOption, "", 1, StringValue(pathValue)))
    }

    val ValidShortArgumentDefaultInsensitiveOption = "  %s %s  %s [dEfAuLt: %s].".format(shortOption, capitalArgument, description, stringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultInsensitiveOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultInsensitiveOption)
      assert (opt.get == Option(shortOption, "", 1, StringValue(stringValue)))
    }
  }

  describe("parseOptionDescriptions") {
    val usage = """ usage: prog
                      -o, --option  <o>
                      --another <a>  description [default: x]
                      <a>
                      <another>  description [default: y]"""
    it("should parse correctly a simple usage") {
      assert (PP.parseOptionDescriptions(usage) ==
              List(Option("-o", "--option", 0),
                   Option("", "--another", 1, StringValue("x"))))
    }

    val doc ="""
    -h, --help  Print help message.
    -o FILE     Output file.
    --verbose   Verbose mode."""
    it("should parse options") {
      assert (PP.parseOptionDescriptions(doc) ==
              List(Option("-h", "--help", 0),
                   Option("-o", "", 1, NullValue()),
                   Option("", "--verbose", 0)))
    }
  }

  describe("parsePattern") {
    val options = List(Option("-h", ""), Option("-v", "--verbose"), Option("-f", "--file", 1, StringValue("")))
    val optionalOption = "[ -h ]"
    it("should parse correctly: %s".format(optionalOption)) {
      assert (PP.parsePattern(optionalOption, options) ==
        Required(Optional(Option("-h", ""))))
    }

    val optionalTwoOption = "[ -h | -v ]"
    it("should parse correctly: %s".format(optionalTwoOption)) {
      assert (PP.parsePattern(optionalTwoOption, options) ==
        Required(Optional(Either(Option("-h", ""), Option("-v","--verbose")))))
    }

    val optionalTwoOptionEither = "( -h | -v [ --file <f>])"
    it("should parse correctly: %s".format(optionalTwoOptionEither)) {
      assert (PP.parsePattern(optionalTwoOptionEither, options) ==
        Required(Required(Either(Option("-h", ""),
                                 Required(Option("-v","--verbose"),
                                          Optional(Option("-f","--file",1, StringValue(""))))))))
    }

    val complexOptions = "(-h|-v[--file=<f>]N...)"
    it("should parse correctly: %s".format(complexOptions)) {
        assert (PP.parsePattern(complexOptions, options) ==
          Required(Required(Either(Option("-h", ""),
                                   Required(Option("-v", "--verbose"),
                                            Optional(Option("-f", "--file", 1, StringValue(""))),
                                            OneOrMore(Argument("N")))))))
    }

    val optionalArgOneOrMany = "[ ARG ... ]"
    it("should parse correctly: %s".format(optionalArgOneOrMany)) {
        assert (PP.parsePattern(optionalArgOneOrMany, options) ==
                Required(Optional(OneOrMore(Argument("ARG")))))
    }

    val optionalOptionOrOptionalArg = "[ -h ] [N]"
    it("should parse correctly: %s".format(optionalOptionOrOptionalArg)) {
      assert (PP.parsePattern(optionalOptionOrOptionalArg, options) ==
              Required(Optional(Option("-h","")), Optional(Argument("N"))))
    }

    val optionOptionalAny = "-v [options]"
    it("should parse correctly: %s".format(optionOptionalAny)) {
        assert (PP.parsePattern(optionOptionalAny, options) ==
                Required(Option("-v","--verbose"), Optional(AnyOptions())))
    }

    val complexEither = "(N [M | (K | L)] | O P)"
    it("should parse correctly: %s".format(complexEither)) {
        assert (PP.parsePattern(complexEither, options) ==
          Required(Required(Either(Required(Argument("N"),
                                            Optional(Either(Argument("M"),
                                                            Required(Either(Argument("K"), Argument("L")))))),
                                   Required(Argument("O"), Argument("P"))))))
    }

    val optionalOptions = "[options]"
    it("should parse correctly: %s".format(optionalOptions)) {
        assert (PP.parsePattern(optionalOptions, options) ==
          Required(Optional(AnyOptions())))
    }

    val optionalOptionsAndArg = "[options] A"
    it("should parse correctly: %s".format(optionalOptionsAndArg)) {
        assert (PP.parsePattern(optionalOptionsAndArg, options) ==
          Required(Optional(AnyOptions()), Argument("A")))
    }

    val capitalArg = "ADD"
    it("should parse correctly: %s".format(capitalArg)) {
        assert (PP.parsePattern(capitalArg, options) == Required(Argument("ADD")))
    }

    val bracketArg= "<arg>"
    it("should parse correctly: %s".format(bracketArg)) {
        assert (PP.parsePattern(bracketArg, options) == Required(Argument("<arg>")))
    }

    val command = "arg"
    it("should parse correctly: %s".format(command)) {
        assert (PP.parsePattern(command, options) == Required(Command("arg")))
    }
  }

  describe("parseArgv") {
    val options = List(Option("-h", ""),
                       Option("-v", "--verbose"),
                       Option("-f", "--file", 1))
    it("should parse correctly: %s".format("")) {
        assert (PP.parseArgv("", options) == Nil)
    }

    it("should parse correctly: %s".format("-h")) {
        assert (PP.parseArgv("-h", options) ==
          List(Option("-h","",0,BooleanValue(value = true))))
    }

    it("should parse correctly: %s".format("-h --verbose")) {
        assert (PP.parseArgv("-h --verbose", options) ==
          List(Option("-h","",0,BooleanValue(value = true)),
               Option("-v","--verbose",0,BooleanValue(value = true))))
    }

    it("should parse correctly: %s".format("-h --file f.txt")) {
        assert (PP.parseArgv("-h --file f.txt", options) ==
          List(Option("-h","",0,BooleanValue(value = true)),
               Option("-f","--file",1,StringValue("f.txt"))))
    }

    it("should parse correctly: %s".format("-h --file f.txt arg")) {
        assert (PP.parseArgv("-h --file f.txt arg", options) ==
          List(Option("-h","",0,BooleanValue(value = true)),
               Option("-f","--file",1,StringValue("f.txt")),
               Argument("", StringValue("arg"))))
    }

    it("should parse correctly: %s".format("-h --file f.txt arg arg2")) {
        assert (PP.parseArgv("-h --file f.txt arg arg2", options) ==
          List(Option("-h","",0,BooleanValue(value = true)),
               Option("-f","--file",1,StringValue("f.txt")),
               Argument("", StringValue("arg")),
               Argument("", StringValue("arg2"))))
    }

    it("should parse correctly: %s".format("-h arg -- -v")) {
        assert (PP.parseArgv("-h arg -- -v", options) ==
          List(Option("-h","",0,BooleanValue(value = true)),
               Argument("", StringValue("arg")),
               Argument("", StringValue("--")),
               Argument("", StringValue("-v"))))
    }
  }
  describe("long options error handling") {
    it("it should intercept a non existant option") {
      intercept[UnconsumedTokensException] {
        PP.docopt("Usage: prog", "--non-existant", help = false, version = "", optionsFirst = false)
      }
    }

    it("it should intercept a substring of an option") {
      val usage = """Usage: prog [--version --verbose]

  --version
  --verbose"""
      intercept[RuntimeException] {
        PP.docopt(usage, "--ver", help = false, "", optionsFirst = false)
      }
    }

    it("it should intercept a conflicting definition") {
      // since the option is defined to have an argument, the implicit ')' is
      // consumed by the parseOption
      intercept[MissingEnclosureException] {
        PP.docopt("Usage: prog --conflict\n\n--conflict ARG", "", help = false, "", optionsFirst = false)
      }
    }

    it("it should intercept a reversed conflicting definition") {
      intercept[UnexpectedArgumentException] {
        PP.docopt("Usage: prog --long=ARG\n\n --long", "", help = false, "", optionsFirst = false)
      }
    }

    it("it should intercept a missing argument") {
      intercept[MissingArgumentException] {
        PP.docopt("Usage: prog --long ARG\n\n --long ARG", "--long", help = false, "", optionsFirst = false)
      }
    }

    it("it should intercept an unexpected argument ") {
      intercept[UnexpectedArgumentException] {
        val doc = """
Usage: prog --derp

Options:
    --derp"""
        PP.docopt(doc, "--derp=ARG", help = false, "", optionsFirst = false)
      }
    }
  }

  describe("short options error handling") {
    it("it should detect conflicting definitions") {
      intercept[UnparsableOptionException] {
        PP.docopt("Usage: prog -x\n\n-x this\n-x that", "", help = false, "", optionsFirst = false)
      }
    }

    it("it should detect undefined options") {
      intercept[UnconsumedTokensException] {
        PP.docopt("Usage: prog", "-x", help = false, "", optionsFirst = false)
      }
    }

    it("it should detect conflicting definitions with arguments") {
      intercept[MissingEnclosureException] {
        PP.docopt("Usage: prog -x\n\n-x ARG", "", help = false, "", optionsFirst = false)
      }
    }

    it("it should detect missing arguments") {
      intercept[MissingArgumentException] {
        PP.docopt("Usage: prog -x ARG\n\n-x ARG", "-x", help = false, "", optionsFirst = false)
      }
    }
  }

  describe("[]|{}|() matching") {
    it("it should detect missing ]") {
      intercept[MissingEnclosureException] {
        PP.docopt("Usage: prog [a [b]", "", help = false, "", optionsFirst = false)
      }
    }

    it("it should detect extra )") {
      intercept[UnconsumedTokensException] {
        PP.docopt("Usage: prog [a [b] ] c )", "", help = false, "", optionsFirst = false)
      }
    }
  }

  describe("double-dash support") {
    it("it should handle correctly '--'") {
      PP.docopt("Usage: prog [-o] [--] <arg>\n\n-o", "-- -o", help = false, "", optionsFirst = false)
    }

    it("it should handle correctly '--' swapped") {
      PP.docopt("Usage: prog [-o] [--] <arg>\n\n -o", "-o 1", help = false, "", optionsFirst = false)
    }
  }
}
