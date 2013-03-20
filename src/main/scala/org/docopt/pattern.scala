package org.docopt.pattern


// Values
abstract class Value { def +(that: Value):Value}
case class BooleanValue(value: Boolean = false) extends Value {
  def +(that: Value) = that match {
    case BooleanValue(tValue) => BooleanValue(value || tValue)
    case _ => this
  }
}

case class IntValue(value: Int = 0) extends Value {
  def +(that: Value) = that match {
    case IntValue(tValue) => IntValue(value + tValue)
    case _ => this
  }
}

case class DoubleValue(value: Double = 0.0) extends Value {
  def +(that: Value) = that match {
    case DoubleValue(tValue) => DoubleValue(value + tValue)
    case _ => this
  }
}

case class StringValue(value: String = "") extends Value {
  def +(that: Value) = that match {
    case StringValue(tValue) => StringValue(value + tValue)
    case _ => this
  }
}

case class ManyStringValue(value: List[String] = Nil) extends Value {
  def +(that: Value) = that match {
    case ManyStringValue(tValue) => ManyStringValue(value ++ tValue)
    case _ => this
  }
}

// Basic Patterns
trait ChildPattern {def value: Value; def name: String}
trait ParentPattern {def children: List[Pattern]}
abstract class Pattern
case class Argument(name: String,
                    value: Value = StringValue()) extends Pattern with ChildPattern
case class Command(name: String,
                   value: Value = BooleanValue(false)) extends Pattern with ChildPattern
case class Option(short: String,
                  long: String,
                  count: Int = 0,
                  value: Value = BooleanValue(false)) extends Pattern with ChildPattern{
  def name: String = if (short != "") short else long
}


// Composed Patterns
case class Required(children: List[Pattern]) extends Pattern with ParentPattern
case class Optional(children: List[Pattern]) extends Pattern with ParentPattern
case class AnyOptions(children: List[Pattern] = Nil) extends Pattern with ParentPattern
case class OneOrMore(children: List[Pattern]) extends Pattern with ParentPattern
case class Either(children: List[Pattern]) extends Pattern with ParentPattern
