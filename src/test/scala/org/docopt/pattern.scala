package org.docopt
import org.scalatest.FunSpec

package object pattern {
  // string references
  val capitalArgument = "ZEARGUMENT"
  val bracketArgument = "<zeargument>"
  val stringValue = "ze_value"
  val doubleValue = "3.14159"
  val intValue = "2"
  val pathValue = "./"
  val zeCommand = "ze_command"
  val shortOption = "-a"
  val longOption = "--zeoption"
  val description = "Ze description"

  // helper for matching
  val namedArgumentPattern = Argument("M")
  val otherNamedArgumentPattern = Argument("N")
  val argumentPattern = Argument("", StringValue(intValue))
  val otherArgumentPattern = Argument("", StringValue(doubleValue))
  val namedArgumentCollectedPattern = Argument("M", StringValue(intValue))
  val otherNamedArgumentCollectedPattern = Argument("N", StringValue(doubleValue))
  val optionPattern = Option(shortOption,"")
  val otherOption = "-b"
  val otherOptionPattern = Option(otherOption,"")
  val manyPattern = List(otherOptionPattern, optionPattern, argumentPattern)
  val RequiredPattern  = Required(optionPattern)
}
