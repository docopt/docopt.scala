package org.docopt

import io.Source
import com.twitter.json.Json

/**
 * Helper program that read the usage documentation from standard input and match it to
 * the received arguments.
 */
object Testee extends App {
  val input = Source.fromInputStream(System.in)
  val doc = input.getLines().mkString("\n")
  try {
    println(Json.build(Docopt(doc, args)))
  }
  catch {
    case _:Throwable => println("\"user-error\"")
  }
}
