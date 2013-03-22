package org.docopt

import org.docopt.{PatternMatcher => PM}
import org.docopt.pattern._
import org.scalatest.FunSpec

class PatternMatcherFunSpec extends FunSpec {
  describe("An Argument") {
    val combinedArgumentPattern = List(Argument(capitalArgument, StringValue(intValue)))
    it("should match itself and extract the value") {
      assert (PM.matchPattern(Argument(capitalArgument), List(argumentPattern)) ==
        Some(Nil, combinedArgumentPattern))
    }

    it("shout not match an option") {
      assert (PM.matchPattern(Argument(capitalArgument), List(optionPattern)) == None)
    }

    it("should match a list of patterns") {
      assert (PM.matchPattern(Argument(capitalArgument), manyPattern) ==
        Some(List(otherOptionPattern, optionPattern), combinedArgumentPattern))
    }

    it("should match a list of itself and consume only once") {
      assert (PM.matchPattern(Argument(capitalArgument),
                              List(argumentPattern, otherArgumentPattern)) ==
        Some(List(otherArgumentPattern), combinedArgumentPattern))
    }
  }

  describe("A Command") {
    // Matching
    val commandPatternMatched = Command(capitalArgument, BooleanValue(value = true))
    it("should match an argument with the same name") {
      assert (PM.matchPattern(Command(capitalArgument),
                              List(Argument("", StringValue(capitalArgument)))) ==
              Some(Nil, List(commandPatternMatched)))
    }

    it("should not match an option") {
      assert (PM.matchPattern(Command(capitalArgument,StringValue()),
                              List(optionPattern)) == None)
    }

    val manyPatternCommand = List(otherOptionPattern,
                                  optionPattern,
                                  Argument("", StringValue(capitalArgument)))
    it("should match a list of patterns") {
      assert (PM.matchPattern(Command(capitalArgument,StringValue()),
                              manyPatternCommand) ==
        Some(List(otherOptionPattern, optionPattern), List(commandPatternMatched)))
    }
  }

  describe("An Option") {
    it("should match itself") {
      assert (PM.matchPattern(Option(shortOption,""), List(optionPattern)) ==
        Some((Nil, List(optionPattern))))
    }

    it("should not match another option") {
      assert (PM.matchPattern(Option(shortOption,""), List(otherOptionPattern)) == None)
    }

    it("should not match an argument") {
      assert (PM.matchPattern(Option(shortOption,""), List(argumentPattern)) == None)
    }

    it("should match a list of patterns") {
      assert (PM.matchPattern(Option(shortOption,""), manyPattern) ==
        Some(List(otherOptionPattern, argumentPattern), List(optionPattern)))
    }

    it("should match a list of itself and consume only once") {
      assert (PM.matchPattern(Option(shortOption,""),
                              List(optionPattern, optionPattern)) ==
        Some(List(optionPattern), List(optionPattern)))
    }
  }

  describe("A Required") {
    it("should match one option alone") {
      assert (PM.matchPattern(Required(optionPattern), List(optionPattern)) ==
        Some(Nil, List(optionPattern)))
    }

    it("should not match an empty list") {
      assert (PM.matchPattern(Required(optionPattern), Nil) == None)
    }

    it("should not match another option") {
      assert (PM.matchPattern(Required(optionPattern), List(otherOptionPattern)) ==
        None)
    }

    it("should not match if only 1 out of 2 options are there") {
      assert (PM.matchPattern(Required(optionPattern, otherOptionPattern),
                              List(otherOptionPattern)) ==
        None)
    }

    it("should match 2 options") {
      assert (PM.matchPattern(Required(optionPattern, otherOptionPattern),
                              manyPattern) ==
        Some(List(argumentPattern), List(optionPattern, otherOptionPattern)))
    }
  }

  describe("An Optional") {
    it("should match one option alone") {
      assert (PM.matchPattern(Optional(optionPattern), List(optionPattern)) ==
        Some(Nil, List(optionPattern)))
    }

    it("should match an empty list") {
      assert (PM.matchPattern(Optional(optionPattern), Nil) ==
        Some(Nil, Nil))
    }

    it("should match another option but not consume it") {
      assert (PM.matchPattern(Optional(optionPattern), List(otherOptionPattern)) ==
        Some(List(otherOptionPattern), Nil))
    }

    it("with 2 patterns should match if only the first is present") {
      assert (PM.matchPattern(Optional(optionPattern, otherOptionPattern),
                              List(optionPattern)) ==
        Some(Nil, List(optionPattern)))
    }

    it("with 2 patterns should match if only the second is present") {
      assert (PM.matchPattern(Optional(optionPattern, otherOptionPattern),
                              List(otherOptionPattern)) ==
        Some(Nil, List(otherOptionPattern)))
    }

    it("with 2 patterns should match if neither is present") {
      assert (PM.matchPattern(Optional(optionPattern, otherOptionPattern),
                              List(argumentPattern)) ==
        Some(List(argumentPattern), Nil))
    }

    val combinedArgumentPattern = List(Argument(capitalArgument, StringValue(intValue)))
    it("with an argument should match it and extract the value") {
      assert (PM.matchPattern(Required(Argument(capitalArgument)), List(argumentPattern)) ==
        Some(Nil, combinedArgumentPattern))
    }

  }

  describe("An Either") {
    it("with 2 options should match when one is provided") {
      assert (PM.matchPattern(Either(optionPattern, otherOptionPattern),
                              List(optionPattern)) ==
        Some(Nil, List(optionPattern)))
    }

    it("with 2 options should match only one out of 2 options") {
      assert (PM.matchPattern(Either(optionPattern, otherOptionPattern),
                              List(optionPattern, otherOptionPattern)) ==
        Some(List(otherOptionPattern), List(optionPattern)))
    }

    it("with 2 options should match not match an argument") {
      assert (PM.matchPattern(Either(optionPattern, otherOptionPattern),
                              List(argumentPattern)) ==
        None)
    }

    val thirdOptionPattern = Option("-c","")
    val randomOptionPattern = Option("-x","")
    it("with 3 options should match only one out of 2 options") {
      assert (PM.matchPattern(Either(optionPattern,
                                     otherOptionPattern,
                                     thirdOptionPattern),
                              List(otherOptionPattern, randomOptionPattern))  ==
        Some(List(randomOptionPattern), List(otherOptionPattern)))
    }

    val complexEitherPattern = Either(namedArgumentPattern,
                                      Required(namedArgumentPattern,
                                               otherNamedArgumentPattern))

    it("with one argument M or a Required of 2 arguments M and N should match 2 arguments and collect them") {
      assert (PM.matchPattern(complexEitherPattern,
                              List(argumentPattern, otherArgumentPattern)) ==
        Some(Nil, List(namedArgumentCollectedPattern, otherNamedArgumentCollectedPattern)))
    }
  }

  describe("A OneOrMore") {
    it("with 1 argument should match an argument") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              List(argumentPattern)) ==
        Some(Nil, List(namedArgumentCollectedPattern)))
    }

    it("with 1 argument should not match an empty list") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern), Nil) == None)
    }

    it("with 1 argument should not match an option") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              List(optionPattern)) == None)
    }

    it("with 1 argument should match 2 arguments and collect them both") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              List(argumentPattern, otherArgumentPattern)) ==
        Some(Nil, List(namedArgumentCollectedPattern,
                       Argument("M", StringValue(doubleValue)))))
    }

    it("with 1 argument should match 2 arguments, collect them both and leave options") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              List(argumentPattern,
                                   optionPattern,
                                   otherArgumentPattern)) ==
        Some(List(optionPattern), List(namedArgumentCollectedPattern,
                                       Argument("M", StringValue(doubleValue)))))
    }

    it("with 1 option should match 2 option, collect them both and leave the argument") {
      assert (PM.matchPattern(OneOrMore(optionPattern),
                              List(optionPattern,
                                   argumentPattern,
                                   optionPattern)) ==
        Some(List(argumentPattern), List(optionPattern, optionPattern)))
    }

    it("with 1 option should not match another option and an argument") {
      assert (PM.matchPattern(OneOrMore(optionPattern),
                              List(argumentPattern,
                                   otherOptionPattern)) == None)
    }

    it("with 1 Required(Option, Argument) should match a complex statement") {
      assert (PM.matchPattern(OneOrMore(Required(optionPattern,
                                                 namedArgumentPattern)),
                              List(optionPattern,
                                   argumentPattern,
                                   otherOptionPattern,
                                   optionPattern,
                                   otherArgumentPattern)) ==
        Some(List(otherOptionPattern),
             List(optionPattern,
                  namedArgumentCollectedPattern,
                  optionPattern,
                  Argument("M", StringValue(doubleValue)))))
    }

    it("with 1 Optional(Argument) should match an argument") {
      assert (PM.matchPattern(OneOrMore(Optional(namedArgumentPattern)),
                              List(argumentPattern)) ==
        Some(Nil, List(namedArgumentCollectedPattern)))
    }
  }

  val pattern = Required(optionPattern,
                         namedArgumentPattern,
                         Optional(otherOptionPattern,
                                  otherNamedArgumentPattern))

  describe("A %s".format(pattern.toString)) {
    val toMatch = List(optionPattern, argumentPattern)
    it("should match %s".format(toMatch.toString())) {
      assert (PM.matchPattern(pattern, toMatch) ==
        Some(Nil, List(optionPattern, namedArgumentCollectedPattern)))
    }

    val toMatch2 = List(optionPattern,
                        otherOptionPattern,
                        argumentPattern,
                        otherArgumentPattern)
    it("should match %s".format(toMatch2.toString())) {
      assert (PM.matchPattern(pattern, toMatch2) ==
        Some(Nil, List(optionPattern, namedArgumentCollectedPattern,
                       otherOptionPattern, otherNamedArgumentCollectedPattern)))
    }

    val toFail = List(otherOptionPattern,
                      argumentPattern,
                      otherArgumentPattern)
    it("should not match %s".format(toFail.toString())) {
      assert (PM.matchPattern(pattern, toFail) == None)
    }
  }
}
