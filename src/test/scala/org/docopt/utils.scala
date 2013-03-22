package org.docopt.utils

import org.docopt.pattern._
import org.scalatest.FunSpec

class UtilsSuite extends FunSpec {
  describe("samePattern") {
    it("it should return false on distinct pattern") {
      assert(!samePattern(AnyOptions())(Option("","")))
    }

    it("it should return true on same pattern") {
      assert(samePattern(AnyOptions())(AnyOptions()))
    }
  }
}

class PatternFlatteningSuite extends FunSpec {
  describe("flattenPattern") {
    it("should flatten correctly with no types") {
      assert(flattenPattern(Required(OneOrMore(argumentPattern),
                                     optionPattern,
                                     otherArgumentPattern)) ==
             List(argumentPattern, optionPattern, otherArgumentPattern))
    }
    it("should flatten correctly with types") {
      assert(flattenPattern(Required(Optional(AnyOptions()),
                                     Optional(optionPattern)),
                            List(AnyOptions())) ==
             List(AnyOptions()))
    }
  }
}

class PatternFixingSuite extends FunSpec {
  describe("fixRepeatedPattern") {
    it("should not touch one Option alone") {
      assert(fixRepeatingArgument(optionPattern) == optionPattern)
    }

    it("should not touch one Argument alone") {
      assert(fixRepeatingArgument(argumentPattern) == argumentPattern)
    }

    it("should accumulate repeated args") {
      assert(fixRepeatingArgument(Required(Argument("N"), Argument("N"))) ==
        Required(Argument("N", ManyStringValue()), Argument("N", ManyStringValue())))
    }

    it("should accumulate a complex pattern with OneOrMore") {
      assert(fixRepeatingArgument(Either(Argument("N"), OneOrMore(Argument("N")))) ==
        Either(Argument("N", ManyStringValue()), OneOrMore(Argument("N", ManyStringValue()))))
      }
  }
}

class EitherPatternSuite extends FunSpec {
  describe("eitherPattern") {
    it("should handle an Option correctly") {
      assert(eitherPattern(optionPattern) == Either(Required(optionPattern)))
    }

    it("should handle an Argument correctly") {
      assert(eitherPattern(argumentPattern) == Either(Required(argumentPattern)))
    }

    it("should handle a Required(Either()) correctly") {
      assert(eitherPattern(Required(Either(optionPattern, otherOptionPattern))) ==
        Either(Required(optionPattern), Required(otherOptionPattern)))
    }

    it("should handle an Optional(Either()) correctly") {
      assert(eitherPattern(Optional(optionPattern, Either(otherOptionPattern, argumentPattern))) ==
        Either(Required(otherOptionPattern, optionPattern),
               Required(argumentPattern, optionPattern)))
    }

    it("should handle an Either(Either()) correctly") {
      assert(eitherPattern(Either(optionPattern, Either(otherOptionPattern, argumentPattern))) ==
        Either(Required(optionPattern), Required(otherOptionPattern), Required(argumentPattern)))
    }

    it("should handle a OneOrMore(Either()) correctly")  {
      assert(eitherPattern(OneOrMore(argumentPattern, otherArgumentPattern)) ==
        Either(Required(argumentPattern, otherArgumentPattern, argumentPattern, otherArgumentPattern)))
    }

  }
}
