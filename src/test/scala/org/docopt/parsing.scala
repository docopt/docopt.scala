package org.docopt.parsing

import org.docopt.parsing.{PatternParser => PP}
import org.docopt.pattern._
import org.scalatest.FunSpec

class ParsingPatternSuite extends FunSpec {
  describe("An Argument") {
    // Parsing
    val ValidArgument = bracketArgument
    val ValidcapitalArgument = capitalArgument
    it("should parse correctly: '%s'".format(ValidArgument)) {
      val arg = PP.parseArgument(ValidArgument)
      assert (arg.get == Argument(ValidArgument, StringValue("")))
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
      assert (opt.get == new Option(shortOption, ""))
    }

    val ValidlongOption = "  %s  %s.".format(longOption, description)
    it("should parse correctly: '%s'".format(ValidlongOption)) {
      val opt = PP.parseOption(ValidlongOption)
      assert (opt.get == new Option("", longOption))
    }

    val ValidShortlongOption = "  %s %s  %s.".format(shortOption, longOption, description)
    it("should parse correctly: '%s'".format(ValidShortlongOption)) {
      val opt = PP.parseOption(ValidShortlongOption)
      assert (opt.get == new Option(shortOption, longOption))
    }

    val ValidShortCommalongOption = "  %s, %s  %s.".format(shortOption, longOption, description)
    it("should parse correctly: '%s'".format(ValidShortCommalongOption)) {
      val opt = PP.parseOption(ValidShortCommalongOption)
      assert (opt.get == new Option(shortOption, longOption))
    }

    val ValidShortLongInverseOption = "  %s %s  %s.".format(longOption, shortOption, description)
    it("should parse correctly: '%s'".format(ValidShortLongInverseOption)) {
      val opt = PP.parseOption(ValidShortLongInverseOption)
      assert (opt.get == new Option(shortOption, longOption))
    }

    val ValidShortSpaceArgumentOption = "  %s %s  %s".format(shortOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortSpaceArgumentOption)) {
      val opt = PP.parseOption(ValidShortSpaceArgumentOption)
      assert (opt.get == new Option(shortOption, "", 1))
    }

    val ValidShortEqualArgumentOption = "  %s=%s  %s".format(shortOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortEqualArgumentOption)) {
      val opt = PP.parseOption(ValidShortEqualArgumentOption)
      assert (opt.get == new Option(shortOption, "", 1))
    }

    val ValidLongSpaceArgumentOption = "  %s %s  %s".format(longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidLongSpaceArgumentOption)) {
      val opt = PP.parseOption(ValidLongSpaceArgumentOption)
      assert (opt.get == new Option("", longOption, 1))
    }

    val ValidShortLongArgumentOption = "  %s %s %s %s  %s.".format(shortOption, capitalArgument, longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortLongArgumentOption)) {
      val opt = PP.parseOption(ValidShortLongArgumentOption)
      assert (opt.get == new Option(shortOption, longOption, 1))
    }

    val ValidShortCommaLongArgumentOption = "  %s %s, %s %s  %s.".format(shortOption, capitalArgument, longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortCommaLongArgumentOption)) {
      val opt = PP.parseOption(ValidShortCommaLongArgumentOption)
      assert (opt.get == new Option(shortOption, longOption, 1))
    }

    val ValidShortCommaLongEqualArgumentOption = "  %s %s, %s=%s  %s.".format(shortOption, capitalArgument, longOption, capitalArgument, description)
    it("should parse correctly: '%s'".format(ValidShortCommaLongEqualArgumentOption)) {
      val opt = PP.parseOption(ValidShortCommaLongEqualArgumentOption)
      assert (opt.get == new Option(shortOption, longOption, 1))
    }

    val ValidShortArgumentDefaultOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, stringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultOption)
      assert (opt.get == new Option(shortOption, "", 1, StringValue(stringValue)))
    }

    val ValidShortArgumentDefaultIntOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, intValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultIntOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultIntOption)
      assert (opt.get == new Option(shortOption, "", 1, IntValue(intValue.toInt)))
    }

    val ValidShortArgumentDefaultFloatOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, doubleValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultFloatOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultFloatOption)
      assert (opt.get == new Option(shortOption, "", 1, DoubleValue(doubleValue.toDouble)))
    }

    val ValidShortArgumentDefaultPathOption = "  %s %s  %s [default: %s].".format(shortOption, capitalArgument, description, pathValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultPathOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultPathOption)
      assert (opt.get == new Option(shortOption, "", 1, StringValue(pathValue)))
    }

    val ValidShortArgumentDefaultInsensitiveOption = "  %s %s  %s [dEfAuLt: %s].".format(shortOption, capitalArgument, description, stringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultInsensitiveOption)) {
      val opt = PP.parseOption(ValidShortArgumentDefaultInsensitiveOption)
      assert (opt.get == new Option(shortOption, "", 1, StringValue(stringValue)))
    }
  }
}
