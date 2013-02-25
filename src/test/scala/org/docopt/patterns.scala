import org.scalatest.FunSpec
import org.docopt.matching.{PatternMatcher => PM}
import org.docopt.parsing.{PatternParser => PP}
import org.docopt.patterns._

class BasicNewPatternSuite extends FunSpec {
  // string references
  val capitalArgument = "ZEARGUMENT"
  val bracketArgument = "<zeargument>"
  val stringValue = "ze_value"
  val doubleValue = "3.14159"
  val intValue = "2"
  val pathValue = "./"
  val zeCommand = "ze_command"
  val shortOption = "-o"
  val longOption = "--zeoption"
  val description = "Ze description"

  // helper for matching
  val argumentPattern = List(Argument("", IntValue(intValue.toInt)))
  val otherArgumentPattern = List(Argument("", DoubleValue(doubleValue.toDouble)))
  val optionPattern = List(Option(shortOption,""))
  val otherOption = "-x"
  val otherOptionPattern = List(Option(otherOption,""))
  val manyPattern = otherOptionPattern ::: optionPattern ::: argumentPattern
  val RequiredPattern  = List(Required(optionPattern))

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

    // Matching
    val CombinedargumentPattern = List(Argument(capitalArgument, IntValue(intValue.toInt)))
    it("should match itself and extract the value") {
      assert (PM.matchPattern(Argument(capitalArgument), argumentPattern) ==
        Some(Nil, CombinedargumentPattern))
    }

    it("shout not match an option") {
      assert (PM.matchPattern(Argument(capitalArgument), optionPattern) == None)
    }

    it("should match a list of patterns") {
      assert (PM.matchPattern(Argument(capitalArgument), manyPattern) ==
        Some(otherOptionPattern ::: optionPattern, CombinedargumentPattern))
    }

    it("should match a list of itself and consume only once") {
      assert (PM.matchPattern(Argument(capitalArgument),
                              argumentPattern ::: otherArgumentPattern) ==
        Some(otherArgumentPattern, CombinedargumentPattern))
    }
  }

  describe("A Command") {
    // Parsing
    // Comand are never parsed, it's only a special case of Argument

    // Matching
    val CommandPatternMatched = List(Command(capitalArgument,
                                             BooleanValue(true)))
    it("should match an argument with the same name") {
      assert (PM.matchPattern(Command(capitalArgument),
                              List(Argument("", StringValue(capitalArgument)))) ==
              Some(Nil, CommandPatternMatched))
    }

    it("should not match an option") {
      assert (PM.matchPattern(Command(capitalArgument,StringValue()),
                              optionPattern) == None)
    }

    val manyPatternCommand =
      otherOptionPattern :::
      optionPattern :::
      List(Argument("", StringValue(capitalArgument)))
    it("should match a list of patterns") {
      assert (PM.matchPattern(Command(capitalArgument,StringValue()),
                              manyPatternCommand) ==
        Some(otherOptionPattern ::: optionPattern, CommandPatternMatched))
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

    // // Matching
    it("should match itself") {
      assert (PM.matchPattern(Option(shortOption,""), optionPattern) ==
        Some((Nil, optionPattern)))
    }

    it("should not match another option") {
      assert (PM.matchPattern(Option(shortOption,""), otherOptionPattern) == None)
    }

    it("should not match an argument") {
      assert (PM.matchPattern(Option(shortOption,""), argumentPattern) == None)
    }

    it("should match a list of patterns") {
      assert (PM.matchPattern(Option(shortOption,""), manyPattern) ==
        Some(otherOptionPattern ::: argumentPattern, optionPattern))
    }

    it("should match a list of itself and consume only once") {
      assert (PM.matchPattern(Option(shortOption,""),
                              optionPattern ::: optionPattern) ==
        Some(optionPattern, optionPattern))
    }
  }

  describe("A Required") {
    it("should match one option alone") {
      assert (PM.matchPattern(Required(optionPattern), optionPattern) ==
        Some(Nil, optionPattern))
    }

    it("should not match an empty list") {
      assert (PM.matchPattern(Required(optionPattern), Nil) == None)
    }

    it("should not match another option") {
      assert (PM.matchPattern(Required(optionPattern), otherOptionPattern) ==
        None)
    }

    it("should not match if only 1 out of 2 options are there") {
      assert (PM.matchPattern(Required(optionPattern ::: otherOptionPattern),
                              otherOptionPattern) ==
        None)
    }

    it("should match 2 options") {
      assert (PM.matchPattern(Required(optionPattern ::: otherOptionPattern),
                              manyPattern) ==
        Some(argumentPattern, optionPattern ::: otherOptionPattern))
    }
  }

  describe("An Optional") {
    it("should match one option alone") {
      assert (PM.matchPattern(Optional(optionPattern), optionPattern) ==
        Some(Nil, optionPattern))
    }

    it("should match an empty list") {
      assert (PM.matchPattern(Optional(optionPattern), Nil) ==
        Some(Nil, Nil))
    }

    it("should match another option but not consume it") {
      assert (PM.matchPattern(Optional(optionPattern), otherOptionPattern) ==
        Some(otherOptionPattern, Nil))
    }

    it("with 2 patterns should match if only the first is present") {
      assert (PM.matchPattern(Optional(optionPattern ::: otherOptionPattern),
                              optionPattern) ==
        Some(Nil, optionPattern))
    }

    it("with 2 patterns should match if only the second is present") {
      assert (PM.matchPattern(Optional(optionPattern ::: otherOptionPattern),
                              otherOptionPattern) ==
        Some(Nil, otherOptionPattern))
    }

    it("with 2 patterns should match if neither is present") {
      assert (PM.matchPattern(Optional(optionPattern ::: otherOptionPattern),
                              argumentPattern) ==
        Some(argumentPattern, Nil))
    }

    val CombinedargumentPattern = List(Argument(capitalArgument, IntValue(intValue.toInt)))
    it("with an argument should match it and extract the value") {
      assert (PM.matchPattern(Required(List(Argument(capitalArgument))), argumentPattern) ==
        Some(Nil, CombinedargumentPattern))
    }

  }
}
