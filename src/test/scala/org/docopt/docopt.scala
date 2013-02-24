import org.scalatest.FunSpec
import org.docopt.{Argument,Command,CmdOption}

class BasicPatternSuite extends FunSpec {
  // string references
  val CapitalArgument = "ZEARGUMENT"
  val BracketArgument = "<zeargument>"
  val StringValue = "ze_value"
  val FloatValue = "3.14159"
  val IntValue = "2"
  val PathValue = "./"
  val ZeCommand = "ze_command"
  val ShortOption = "-o"
  val LongOption = "--zeoption"
  val Description = "Ze description"

  // helper for matching
  val ArgumentPattern = List(new Argument("", IntValue))
  val OtherArgumentPattern = List(new Argument("", FloatValue))
  val OptionPattern = List(new CmdOption(ShortOption,""))
  val OtherOption = "-x"
  val OtherOptionPattern = List(new CmdOption(OtherOption,""))
  val ManyPattern = OtherOptionPattern ::: OptionPattern ::: ArgumentPattern

  describe("An Argument") {
    // Parsing
    val ValidArgument = BracketArgument
    val ValidCapitalArgument = CapitalArgument
    it("should parse correctly: '%s'".format(ValidArgument)) {
      val arg = Argument.createFromString(ValidArgument)
      assert (arg.get == new Argument(ValidArgument, ""))
    }

    val ValidArgumentDefault = "%s [default: %s]".format(BracketArgument, StringValue)
    it("should parse correctly: '%s'".format(ValidArgumentDefault)) {
      val arg = Argument.createFromString(ValidArgumentDefault)
      assert (arg.get == new Argument(BracketArgument, StringValue))
    }

    // Matching
    val CombinedArgumentPattern = List(new Argument(CapitalArgument, IntValue))
    it("should match itself and extract the value") {
      assert ((new Argument(CapitalArgument,"")).matchPattern(ArgumentPattern) ==
        Some(Nil, CombinedArgumentPattern))
    }

    it("shout not match an option") {
      assert ((new Argument(CapitalArgument,"")).matchPattern(OptionPattern) == None)
    }

    it("should match a list of patterns") {
      assert ((new Argument(CapitalArgument,"")).matchPattern(ManyPattern) ==
        Some(OtherOptionPattern ::: OptionPattern, CombinedArgumentPattern))
    }

    it("should match a list of itself and consume only once") {
      assert ((new Argument(CapitalArgument,"")).matchPattern(ArgumentPattern ::: OtherArgumentPattern) ==
        Some(OtherArgumentPattern, CombinedArgumentPattern))
    }
  }

  describe("A Command") {
    // Parsing
    val ValidCommand = ZeCommand
    it("should parse correctly: '%s'".format(ValidCommand)) {
      val cmd = new Command(ValidCommand, "true")
      assert (cmd.name == ValidCommand)
      assert (cmd.value == "true")
    }

    // Matching
    val CommandPatternMatched = List(new Command(CapitalArgument, "true"))
    it("should match an argument with the same name") {
      assert ((new Command(CapitalArgument,"")).matchPattern(List(new Argument("", CapitalArgument))) ==
        Some(Nil, CommandPatternMatched))
    }

    it("should not match an option") {
      assert ((new Command(CapitalArgument,"")).matchPattern(OptionPattern) == None)
    }

    val ManyPatternCommand = OtherOptionPattern ::: OptionPattern ::: List(new Argument("", CapitalArgument))
    it("should match a list of patterns") {
      assert ((new Command(CapitalArgument,"")).matchPattern(ManyPatternCommand) ==
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

    val ValidShortArgumentDefaultOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, StringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, StringValue))
    }

    val ValidShortArgumentDefaultIntOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, IntValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultIntOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultIntOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, IntValue))
    }

    val ValidShortArgumentDefaultFloatOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, FloatValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultFloatOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultFloatOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, FloatValue))
    }

    val ValidShortArgumentDefaultPathOption = "  %s %s  %s [default: %s].".format(ShortOption, CapitalArgument, Description, PathValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultPathOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultPathOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, PathValue))
    }

    val ValidShortArgumentDefaultInsensitiveOption = "  %s %s  %s [dEfAuLt: %s].".format(ShortOption, CapitalArgument, Description, StringValue)
    it("should parse correctly: '%s'".format(ValidShortArgumentDefaultInsensitiveOption)) {
      val opt = CmdOption.createFromString(ValidShortArgumentDefaultInsensitiveOption)
      assert (opt.get == new CmdOption(ShortOption, "", 1, StringValue))
    }

    // Matching
    it("should match itself") {
      assert ((new CmdOption(ShortOption,"")).matchPattern(OptionPattern) ==
        Some((Nil, OptionPattern)))
    }

    it("should not match another option") {
      assert ((new CmdOption(ShortOption,"")).matchPattern(OtherOptionPattern) == None)
    }

    it("should not match an argument") {
      assert ((new CmdOption(ShortOption,"")).matchPattern(ArgumentPattern) == None)
    }

    it("should match a list of patterns") {
      assert ((new CmdOption(ShortOption,"")).matchPattern(ManyPattern) ==
        Some(OtherOptionPattern ::: ArgumentPattern, OptionPattern))
    }

    it("should match a list of itself and consume only once") {
      assert ((new CmdOption(ShortOption,"")).matchPattern(OptionPattern ::: OptionPattern) ==
        Some(OptionPattern, OptionPattern))
    }
  }
}
