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
  describe("fixPattern") {
    it("") {
    }
  }
}

class EitherPatternSuite extends FunSpec {
  describe("eitherPattern") {
    it("") {
    }
  }
}
