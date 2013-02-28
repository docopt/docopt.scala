package org.docopt.matching

import org.docopt.matching.{PatternMatcher => PM}
import org.docopt.pattern._
import org.scalatest.FunSpec

class SingleMatchingPatternSuite extends FunSpec {
  describe("An Argument") {
    val CombinedargumentPattern = List(Argument(capitalArgument, StringValue(intValue)))
    it("should match itself and extract the value") {
      println(PM.matchPattern(Argument(capitalArgument), argumentPattern))
      println(CombinedargumentPattern)
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

    val CombinedargumentPattern = List(Argument(capitalArgument, StringValue(intValue)))
    it("with an argument should match it and extract the value") {
      assert (PM.matchPattern(Required(List(Argument(capitalArgument))), argumentPattern) ==
        Some(Nil, CombinedargumentPattern))
    }

  }

  describe("An Either") {
    it("with 2 options should match when one is provided") {
      assert (PM.matchPattern(Either(optionPattern ::: otherOptionPattern),
                              optionPattern) ==
        Some(Nil, optionPattern))
    }

    it("with 2 options should match only one out of 2 options") {
      assert (PM.matchPattern(Either(optionPattern ::: otherOptionPattern),
                              optionPattern ::: otherOptionPattern) ==
        Some(otherOptionPattern, optionPattern))
    }

    it("with 2 options should match not match an argument") {
      assert (PM.matchPattern(Either(optionPattern ::: otherOptionPattern),
                              argumentPattern) ==
        None)
    }

    val thirdOptionPattern = List(Option("-c",""))
    val randomOptionPattern = List(Option("-x",""))
    it("with 3 options should match only one out of 2 options") {
      assert (PM.matchPattern(Either(optionPattern :::
                                     otherOptionPattern :::
                                     thirdOptionPattern),
                              otherOptionPattern ::: randomOptionPattern)  ==
        Some(randomOptionPattern, otherOptionPattern))
    }

    val complexEitherPattern = Either(namedArgumentPattern :::
                                      List(Required(namedArgumentPattern :::
                                                    otherNamedArgumentPattern)))

    it("with one argument M or a Required of 2 arguments M and N should match 2 arguments and collect them") {
      assert (PM.matchPattern(complexEitherPattern,
                              argumentPattern ::: otherArgumentPattern) ==
        Some(Nil, namedArgumentCollectedPattern ::: otherNamedArgumentCollectedPattern))
    }
  }

  describe("A OneOrMore") {
    it("with 1 argument should match an argument") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              argumentPattern) ==
        Some(Nil, namedArgumentCollectedPattern))
    }

    it("with 1 argument should not match an empty list") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern), Nil) == None)
    }

    it("with 1 argument should not match an option") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              optionPattern) == None)
    }

    it("with 1 argument should match 2 arguments and collect them both") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              argumentPattern ::: otherArgumentPattern) ==
        Some(Nil, namedArgumentCollectedPattern ::: List(Argument("M",
          StringValue(doubleValue)))))
    }

    it("with 1 argument should match 2 arguments, collect them both and leave options") {
      assert (PM.matchPattern(OneOrMore(namedArgumentPattern),
                              argumentPattern :::
                              optionPattern :::
                              otherArgumentPattern) ==
        Some(optionPattern, namedArgumentCollectedPattern ::: List(Argument("M",
          StringValue(doubleValue)))))
    }

    it("with 1 option should match 2 option, collect them both and leave the argument") {
      assert (PM.matchPattern(OneOrMore(optionPattern),
                              optionPattern :::
                              argumentPattern:::
                              optionPattern) ==
        Some(argumentPattern, optionPattern ::: optionPattern))
    }

    it("with 1 option should not match another option and an argument") {
      assert (PM.matchPattern(OneOrMore(optionPattern),
                              argumentPattern:::
                              otherOptionPattern) == None)
    }

    it("with 1 Required(Option, Argument) should match a complex statement") {
      assert (PM.matchPattern(OneOrMore(List(Required(optionPattern :::
                                                      namedArgumentPattern))),
                              optionPattern :::
                              argumentPattern :::
                              otherOptionPattern :::
                              optionPattern :::
                              otherArgumentPattern) ==
        Some(otherOptionPattern,
             optionPattern :::
             namedArgumentCollectedPattern :::
             optionPattern :::
             List(Argument("M", StringValue(doubleValue)))))
    }

    it("with 1 Optional(Argument) should match an argument") {
      assert (PM.matchPattern(OneOrMore(List(Optional(namedArgumentPattern))),
                              argumentPattern) ==
        Some(Nil, namedArgumentCollectedPattern))
    }
  }
}

class BasicMatchingPatternSuite extends FunSpec {
  val pattern = Required(optionPattern :::
                         namedArgumentPattern :::
                         List(Optional(otherOptionPattern :::
                                       otherNamedArgumentPattern)))

  describe("A %s".format(pattern.toString)) {
    val toMatch = optionPattern ::: argumentPattern
    it("should match %s".format(toMatch.toString)) {
      assert (PM.matchPattern(pattern, toMatch) ==
        Some(Nil, optionPattern ::: namedArgumentCollectedPattern))
    }

    val toMatch2 = optionPattern :::
                   otherOptionPattern :::
                   argumentPattern :::
                   otherArgumentPattern
    it("should match %s".format(toMatch2.toString)) {
      assert (PM.matchPattern(pattern, toMatch2) ==
        Some(Nil, optionPattern ::: namedArgumentCollectedPattern :::
                  otherOptionPattern ::: otherNamedArgumentCollectedPattern))
    }

    val toFail = otherOptionPattern :::
                 argumentPattern :::
                 otherArgumentPattern
    it("should not match %s".format(toFail.toString)) {
      assert (PM.matchPattern(pattern, toFail) == None)
    }
  }
}
