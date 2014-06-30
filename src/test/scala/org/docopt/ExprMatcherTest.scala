package org.docopt

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import org.docopt.{ ExprMatcher => M }


class ExprMatcherTest extends FunSpec with ShouldMatchers {

  describe("An Argument") {

    it("should match and extract the value") {
      M.matchExpr(Argument("arg"), Command("val")) should be (
        Some(Nil, List(Argument("arg", "val"))))
    }

    it("should not match an Option") {
      M.matchExpr(Argument("arg"), Option("-o")) should be (None)
    }

    it("should match a list of patterns") {
      M.matchExpr(Argument("arg"), List(Option("-a"), Command("val"), Option("-b"))) should be (
        Some(List(Option("-a"), Option("-b")), List(Argument("arg", "val"))))
    }

    it("should match a list of patterns and consume once") {
      M.matchExpr(Argument("arg"), List(Option("-a"), Command("val"), Command("val2"))) should be (
        Some(List(Option("-a"), Command("val2")), List(Argument("arg", "val"))))
    }

  }

  describe("A Command") {

    it("should match with same name") {
      M.matchExpr(Command("cmd"), Command("cmd")) should be (
        Some(Nil, List(Command("cmd", Value(value = true)))))
    }

    it("should not match a Command with a different name") {
      M.matchExpr(Command("cmd"), Command("cmd2")) should be (None)
    }

    it("should not match an Option") {
      M.matchExpr(Command("cmd"), Option("-o")) should be (None)
    }

    it("should match a list of patterns") {
      M.matchExpr(Command("cmd"), List(Option("-a"), Command("cmd"), Option("-b"))) should be (
        Some(List(Option("-a"), Option("-b")), List(Command("cmd", Value(value = true)))))
    }

    it("should match a list of patterns and consume once") {
      M.matchExpr(Command("cmd"), List(Option("-a"), Command("cmd"), Command("cmd"))) should be (
        Some(List(Option("-a"), Command("cmd")), List(Command("cmd", Value(value = true)))))
    }

  }

  describe("An Option") {

    it("should match with same name") {
      M.matchExpr(Option("-o"), Option("-o")) should be (Some(Nil, List(Option("-o"))))
    }

    it("should not match another Option") {
      M.matchExpr(Option("-a"), Option("-b")) should be (None)
    }

    it("should not match a Command") {
      M.matchExpr(Option("-o"), Command("-o")) should be (None)
    }

    it("should match a list of patterns") {
      M.matchExpr(Option("-b"), List(Option("-a"), Command("cmd"), Option("-b"))) should be (
        Some(List(Option("-a"), Command("cmd")), List(Option("-b"))))
    }

    it("should match a list of patterns and consume once") {
      M.matchExpr(Option("-o"), List(Option("-o"), Option("-o"))) should be (
        Some(List(Option("-o")), List(Option("-o"))))
    }

  }

  describe("A Required") {

    it("should match an Option") {
      M.matchExpr(Required(Option("-o")), Option("-o")) should be (Some(Nil, List(Option("-o"))))
    }

    it("should not match an Nil") {
      M.matchExpr(Required(Option("-o")), Nil) should be (None)
    }

    it("should not match another Option") {
      M.matchExpr(Required(Option("-o")), Option("-b")) should be (None)
    }

    it("should not match if all children are not satisfied") {
      M.matchExpr(Required((for (o <- 'a' to 'z') yield Option("-" + o)) toList),
                  (for (o <- 'a' to 'y') yield Option("-" + o)).toList) should be (None)
    }

    it("should match if all children are satisfied") {
      M.matchExpr(Required((for (o <- 'a' to 'z') yield Option("-" + o)) toList),
                  (for (o <- 'a' to 'z') yield Option("-" + o)).toList) should be (
        Some(Nil, (for (o <- 'a' to 'z') yield Option("-" + o)).toList.reverse))
    }

    it("should match if all children are satisfied and extras are left") {
      M.matchExpr(Required((for (o <- 'a' to 'y') yield Option("-" + o)) toList),
                  (for (o <- 'a' to 'z') yield Option("-" + o)) toList) should be (
        Some(Option("-z") :: Nil, (for (o <- 'a' to 'y') yield Option("-" + o)).toList.reverse))
    }

  }

  describe("An Optional") {

    it("should match an Option") {
      M.matchExpr(Optional(Option("-o")), Option("-o")) should be (Some(Nil, List(Option("-o"))))
    }

    it("should match an empty list") {
      M.matchExpr(Optional(Option("-o")), Nil) should be (Some(Nil, Nil))
    }

    it("should match another Option") {
      M.matchExpr(Optional(Option("-a")), Option("-b")) should be (Some(List(Option("-b")), Nil))
    }

    it("should match complex Option") {
      M.matchExpr(Optional(Option("-a"), Option("-b"), Option("-c")),
                  List(Option("-d"), Option("-a"))) should be (
        Some(List(Option("-d")), List(Option("-a"))))
    }

  }
}
