package org.docopt.pattern

// Values
abstract class Value
case class NullValue(value: Any = null) extends Value
case class BooleanValue(value: Boolean = false) extends Value
case class IntValue(value: Int = 0) extends Value
case class StringValue(value: String = "") extends Value
case class ManyStringValue(value: List[String] = Nil) extends Value

// Basic Patterns
trait ChildPattern {def value: Value; def name: String}
trait ParentPattern {def children: List[Pattern]}
abstract class Pattern
case class Argument(name: String, value: Value = NullValue()) extends Pattern with ChildPattern
case class Command(name: String, value: Value = BooleanValue(value = false)) extends Pattern with ChildPattern
case class Option(short: String,
                  long: String,
                  count: Int = 0,
                  value: Value = BooleanValue(value = false)) extends Pattern with ChildPattern{
  def name: String = if (long != "") long else short
}

// Composed Patterns
case class Required(children: List[Pattern]) extends Pattern with ParentPattern
case class Optional(children: List[Pattern]) extends Pattern with ParentPattern
case class AnyOptions(children: List[Pattern]) extends Pattern with ParentPattern
case class OneOrMore(children: List[Pattern]) extends Pattern with ParentPattern
case class Either(children: List[Pattern]) extends Pattern with ParentPattern

// Use for syntastic sugar
object Required {def apply(children: Pattern*) = new Required(children.toList)}
object Optional {def apply(children: Pattern*) = new Optional(children.toList)}
object AnyOptions {def apply(children: Pattern*) = new AnyOptions(children.toList)}
object OneOrMore {def apply(children: Pattern*) = new OneOrMore(children.toList)}
object Either {def apply(children: Pattern*) = new Either(children.toList)}
