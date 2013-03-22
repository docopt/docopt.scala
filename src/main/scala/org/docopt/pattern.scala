package org.docopt.pattern

// Values
abstract class Value { def +(that: Value):Value}
case class NullValue(value: Any = null) extends Value { def +(that: Value) = this }
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
                    value: Value = NullValue()) extends Pattern with ChildPattern
case class Command(name: String,
                   value: Value = BooleanValue(value = false)) extends Pattern with ChildPattern
case class Option(short: String,
                  long: String,
                  count: Int = 0,
                  value: Value = BooleanValue(value = false)) extends Pattern with ChildPattern{
  def name: String = if (long != "") long else short
}

// Composed Patterns
case class Required(children: List[Pattern]) extends Pattern with ParentPattern
object Required {def apply(children: Pattern*) = new Required(children.toList)}

case class Optional(children: List[Pattern]) extends Pattern with ParentPattern
object Optional {def apply(children: Pattern*) = new Optional(children.toList)}

case class AnyOptions(children: List[Pattern]) extends Pattern with ParentPattern
object AnyOptions {def apply(children: Pattern*) = new AnyOptions(children.toList)}

case class OneOrMore(children: List[Pattern]) extends Pattern with ParentPattern
object OneOrMore {def apply(children: Pattern*) = new OneOrMore(children.toList)}

case class Either(children: List[Pattern]) extends Pattern with ParentPattern
object Either {def apply(children: Pattern*) = new Either(children.toList)}
