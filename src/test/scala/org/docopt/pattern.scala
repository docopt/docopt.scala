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
  val namedArgumentPattern = List(Argument("M"))
  val otherNamedArgumentPattern = List(Argument("N"))
  val argumentPattern = List(Argument("", StringValue(intValue)))
  val otherArgumentPattern = List(Argument("", StringValue(doubleValue)))
  val namedArgumentCollectedPattern = List(Argument("M", StringValue(intValue)))
  val otherNamedArgumentCollectedPattern = List(Argument("N", StringValue(doubleValue)))
  val optionPattern = List(Option(shortOption,""))
  val otherOption = "-b"
  val otherOptionPattern = List(Option(otherOption,""))
  val manyPattern = otherOptionPattern ::: optionPattern ::: argumentPattern
  val RequiredPattern  = List(Required(optionPattern))
}
