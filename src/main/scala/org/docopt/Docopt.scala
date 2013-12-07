package org.docopt

import org.docopt.pattern._

import collection.mutable.{HashMap, Map}
import org.docopt.pattern.Option
import org.docopt.pattern.Command
import org.docopt.pattern.Argument

object Docopt {

  private def extractValue(value: Value): Any = value match {
    case b:BooleanValue => b.value
    case i:IntValue => i.value
    case s:StringValue => s.value
    case m:ManyStringValue => Array(m.value:_*)
    case _ => null
  }

  def apply(usage: String,
            argv: Array[String],
            help: Boolean = true,
            version: String = "",
            optionsFirst: Boolean = false): Map[String, Any] = {
    val collected = PatternParser.docopt(usage, argv, help, version, optionsFirst)
    val tupled:Seq[(String, Any)] = collected.map(pattern => pattern match {
      case o@Option(l,s,a,value:Value) => (o.name ,extractValue(value))
      case Argument(name,value:Value) => (name, extractValue(value))
      case Command(name,value:Value) => (name, extractValue(value))
    })
    HashMap(tupled:_*)
  }
}
