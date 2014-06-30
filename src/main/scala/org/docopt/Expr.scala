package org.docopt

sealed trait Value
case class BooleanValue(value: Boolean) extends Value
case class IntValue(value: Int) extends Value
case class StringValue(value: List[String]) extends Value

sealed trait Expr

sealed trait NamedExpr extends Expr {
  def name: String
  def value: scala.Option[Value]
}

case class Argument(name: String, value: scala.Option[Value] = None) extends NamedExpr
case class Command(name: String, value: scala.Option[Value] = None) extends NamedExpr
case class Option(name: String,
                  value: scala.Option[Value] = None,
                  argument: scala.Option[Argument] = None) extends NamedExpr

sealed trait ComposedExpr extends Expr {
  def children: List[Expr]
  def map(f: Expr => Expr): ComposedExpr
}

case class Required(children: List[Expr]) extends ComposedExpr {
  def map(f:Expr => Expr) = Required(children map f)
}

case class Optional(children: List[Expr]) extends ComposedExpr {
  def map(f:Expr => Expr) = Optional(children map f)
}

case class AnyOptions(children: List[Expr]) extends ComposedExpr {
  def map(f:Expr => Expr) = AnyOptions(children map f)
}

case class OneOrMore(children: List[Expr]) extends ComposedExpr {
  def map(f:Expr => Expr) = OneOrMore(children map f)
}

case class Either(children: List[Expr]) extends ComposedExpr {
  def map(f:Expr => Expr) = Either(children map f)
}

object Value {
  def apply(value: Boolean) = BooleanValue(value)
  def apply(value: Int) = IntValue(value)
  def apply(value: String) = StringValue(List(value))
  def apply(value: List[String]) = StringValue(value)
}

// Used for syntactic sugar
object Argument { def apply(name: String, value: String) = new Argument(name, Some(Value(value))) }
object Command { def apply(name: String, value: Value) = new Command(name, Some(value)) }
object Option {
  def apply(name: String, arg: Argument) = new Option(name, argument = Some(arg))
  def apply(name: String, value: Value) = new Option(name, value = Some(value))
}
object Required { def apply(children: Expr*) = new Required(children.toList) }
object Optional { def apply(children: Expr*) = new Optional(children.toList) }
object AnyOptions { def apply(children: Expr*) = new AnyOptions(children.toList) }
object OneOrMore { def apply(children: Expr*) = new OneOrMore(children.toList) }
object Either { def apply(children: Expr*) = new Either(children.toList) }

object Expr {
  def map(expr: Expr, f: Expr => Expr) = expr match {
    case c:ComposedExpr => c.map(f)
    case e:Expr => f(e)
  }
}
