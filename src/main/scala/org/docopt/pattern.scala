package org.docopt.pattern

// Values
class Value
case class IntValue(value: Int = 0) extends Value
case class DoubleValue(value: Double = 0.0) extends Value
case class BooleanValue(value: Boolean = false) extends Value
case class StringValue(value: String = "") extends Value

// Basic Patterns
abstract class Pattern
case class Argument(name: String, value: Value = StringValue()) extends Pattern
case class Command(name: String, value: Value = BooleanValue(false)) extends Pattern
case class Option(short: String, long: String, count: Int = 0, value: Value = StringValue()) extends Pattern {
  def name: String = if (short != "") short else long
}

// Composed Patterns
case class Required(children: List[Pattern]) extends Pattern
case class Optional(children: List[Pattern]) extends Pattern
case class AnyOptions(children: List[Pattern] = Nil) extends Pattern
case class OneOrMore(children: List[Pattern]) extends Pattern
case class Either(children: List[Pattern]) extends Pattern
