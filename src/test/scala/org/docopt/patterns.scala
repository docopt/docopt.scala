import org.scalatest.FunSpec
import org.docopt.patterns.{PatternMatcher => PM}
import org.docopt.patterns._

class BasicNewPatternSuite extends FunSpec {
  // string references
  val CapitalArgument = "ZEARGUMENT"
  val BracketArgument = "<zeargument>"
  val stringValue = "ze_value"
  val doubleValue = "3.14159"
  val intValue = "2"
  val pathValue = "./"
  val ZeCommand = "ze_command"
  val ShortOption = "-o"
  val LongOption = "--zeoption"
  val Description = "Ze description"

  // helper for matching
  val ArgumentPattern = List(Argument("", IntValue(intValue.toInt)))
  val OtherArgumentPattern = List(Argument("", DoubleValue(doubleValue.toDouble)))
  val OptionPattern = List(CmdOption(ShortOption,""))
  val OtherOption = "-x"
  val OtherOptionPattern = List(CmdOption(OtherOption,""))
  val ManyPattern = OtherOptionPattern ::: OptionPattern ::: ArgumentPattern

  describe("An Argument") {
    // Parsing
    val ValidArgument = BracketArgument
    val ValidCapitalArgument = CapitalArgument
    it("should parse correctly: '%s'".format(ValidArgument)) {
      val arg = Argument.createFromString(ValidArgument)
      assert (arg.get == Argument(ValidArgument, StringValue("")))
    }

    val ValidArgumentDefault = "%s [default: %s]".format(BracketArgument, stringValue)
    it("should parse correctly: '%s'".format(ValidArgumentDefault)) {
      val arg = Argument.createFromString(ValidArgumentDefault)
      assert (arg.get == Argument(BracketArgument, StringValue(stringValue)))
    }

    // Matching
    val CombinedArgumentPattern = List(Argument(CapitalArgument, IntValue(intValue.toInt)))
    it("should match itself and extract the value") {
      assert (PM.matchPattern(Argument(CapitalArgument), ArgumentPattern) ==
        Some(Nil, CombinedArgumentPattern))
    }

    it("shout not match an option") {
      assert (PM.matchPattern(Argument(CapitalArgument), OptionPattern) == None)
    }

    it("should match a list of patterns") {
      assert (PM.matchPattern(Argument(CapitalArgument), ManyPattern) ==
        Some(OtherOptionPattern ::: OptionPattern, CombinedArgumentPattern))
    }

    it("should match a list of itself and consume only once") {
      assert (PM.matchPattern(Argument(CapitalArgument),
                              ArgumentPattern ::: OtherArgumentPattern) ==
        Some(OtherArgumentPattern, CombinedArgumentPattern))
    }
  }

  describe("A Command") {
    // Parsing
    // Comand are never parsed, it's only a special case of Argument

    // Matching
    val CommandPatternMatched = List(Command(CapitalArgument,
                                             BooleanValue(true)))
    it("should match an argument with the same name") {
      assert (PM.matchPattern(Command(CapitalArgument),
                              List(Argument("", StringValue(CapitalArgument)))) ==
              Some(Nil, CommandPatternMatched))
    }

    it("should not match an option") {
      assert (PM.matchPattern(Command(CapitalArgument,StringValue()),
                              OptionPattern) == None)
    }

    val ManyPatternCommand =
      OtherOptionPattern :::
      OptionPattern :::
      List(Argument("", StringValue(CapitalArgument)))
    it("should match a list of patterns") {
      assert (PM.matchPattern(Command(CapitalArgument,StringValue()),
                              ManyPatternCommand) ==
        Some(OtherOptionPattern ::: OptionPattern, CommandPatternMatched))
    }
  }

  describe("An Option") {
    val ValidShortOption = "  %s  %s.".format(ShortOption, Description)
    it("should parse correctly: '%s'".format(ValidShortOption)) {
      val opt = CmdOption.createFromString(ValidShortOption)
      assert (opt.get == new CmdOption(ShortOption, ""))
    }

    val ValidLongOption = "  %s  %s.".format(LongOption, Description)
    it("should parse correctly: '%s'".format(ValidLongOption)) {
      val opt = CmdOption.createFromString(ValidLongOption)
      assert (opt.get == new CmdOption("", LongOption))
    }

    val ValidShortLongOption = "  %s %s  %s.".format(ShortOption, LongOption, Description)
    it("should parse correctly: '%s'".format(ValidShortLongOption)) {
      val opt = CmdOption.createFromString(ValidShortLongOption)
      assert (opt.get == new CmdOption(ShortOption, LongOption))
    }

    val ValidShortCommaLongOption = "  %s, %s  %s.".format(ShortOption, LongOption, Description)
    it("should parse correctly: '%s'".format(ValidShortCommaLongOption)) {
      val opt = CmdOption.createFromString(ValidShortCommaLongOption)
      assert (opt.get == new CmdOption(ShortOption, LongOption))
    }

    val ValidShortLongInverseOption = "  %s %s  %s.".format(LongOption, ShortOption, Description)
    it("should parse correctly: '%s'".format(ValidShortLongInverseOption)) {
      val opt = CmdOption.createFromString(ValidShortLongInverseOption)
      assert (opt.get == new CmdOption(ShortOption, LongOption))
    }

    val ValidShortSpaceArgumentOption = "  %s %s  %s".format(ShortOption, CapitalArgument, Description)
    it("should parse correctly: '%s'".format(ValidShortSpaceArgumentOption)) {
      val opt = CmdOption.createFromString(ValidShortSpaceArgumentOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1))
    }

    val ValidShortEqualArgumentOption = "  %s=%s  %s".format(ShortOption, CapitalArgument, Description)
    it("should parse correctly: '%s'".format(ValidShortEqualArgumentOption)) {
      val opt = CmdOption.createFromString(ValidShortEqualArgumentOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1))
    }

    val ValidLongSpaceArgumentOption = "  %s %s  %s".format(LongOption, CapitalArgument, Description)
    it("should parse correctly: '%s'".format(ValidLongSpaceArgumentOption)) {
      val opt = CmdOption.createFromString(ValidLongSpaceArgumentOption)
      assert (opt.get == new CmdOption("", LongOption, 1))
    }

    val ValidShortLongArgumentOption = "  %s %s %s %s  %s.".format(ShortOption, CapitalArgument, LongOption, CapitalArgument, Description)
    it("should parse correctly: '%s'".format(ValidShortLongArgumentOption)) {
      val opt = CmdOption.createFromString(ValidShortLongArgumentOption)
      assert (opt.get == new CmdOption(ShortOption, LongOption, 1))
    }

    val ValidShortCommaLongArgumentOption = "  %s %s, %s %s  %s.".format(ShortOption, CapitalArgument, LongOption, CapitalArgument, Description)
    it("should parse correctly: '%s'".format(ValidShortCommaLongArgumentOption)) {
      val opt = CmdOption.createFromString(ValidShortCommaLongArgumentOption)
      assert (opt.get == new CmdOption(ShortOption, LongOption, 1))
    }

    val ValidShortCommaLongEqualArgumentOption = "  %s %s, %s=%s  %s.".format(ShortOption, CapitalArgument, LongOption, CapitalArgument, Description)
    it("should parse correctly: '%s'".format(ValidShortCommaLongEqualArgumentOption)) {
      val opt = CmdOption.createFromString(ValidShortCommaLongEqualArgumentOption)
      assert (opt.get == new CmdOption(ShortOption, LongOption, 1))
    }

    val ValidShortArgumentDefaultOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, stringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, StringValue(stringValue)))
    }

    val ValidShortArgumentDefaultIntOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, intValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultIntOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultIntOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, IntValue(intValue.toInt)))
    }

    val ValidShortArgumentDefaultFloatOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, doubleValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultFloatOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultFloatOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, DoubleValue(doubleValue.toDouble)))
    }

    val ValidShortArgumentDefaultPathOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, pathValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultPathOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultPathOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, StringValue(pathValue)))
    }

    val ValidShortArgumentDefaultInsensitiveOption = "  %s %s  %s [dEfAuLt: %s].".format(ShortOption, CapitalArgument, Description, stringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultInsensitiveOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultInsensitiveOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, StringValue(stringValue)))
    }

    // // Matching
    it("should match itself") {
      assert (PM.matchPattern(CmdOption(ShortOption,""), OptionPattern) ==
        Some((Nil, OptionPattern)))
    }

    it("should not match another option") {
      assert (PM.matchPattern(CmdOption(ShortOption,""), OtherOptionPattern) == None)
    }

    it("should not match an argument") {
      assert (PM.matchPattern(CmdOption(ShortOption,""), ArgumentPattern) == None)
    }

    it("should match a list of patterns") {
      assert (PM.matchPattern(CmdOption(ShortOption,""), ManyPattern) ==
        Some(OtherOptionPattern ::: ArgumentPattern, OptionPattern))
    }

    it("should match a list of itself and consume only once") {
      assert (PM.matchPattern(CmdOption(ShortOption,""),
                              OptionPattern ::: OptionPattern) ==
        Some(OptionPattern, OptionPattern))
    }
  }
}
