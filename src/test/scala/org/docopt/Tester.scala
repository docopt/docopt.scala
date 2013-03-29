package org.docopt

import io.Source
import com.twitter.json.Json

object Tester extends App {
  val testCases = Source.fromURL(getClass.getResource("/testcases.docopt"))
                        .getLines()
                        .filterNot(_.startsWith("#"))
                        .mkString("\n")

  val testSet = Set(args.map(x => Integer.valueOf(x)):_*)
  val testFilter = if (args.length > 0) { x:Integer => testSet.contains(x) }
                   else {x:Integer => true}

  testCases.split("r\"\"\"").zipWithIndex.filter({case (t, x) => testFilter(x)}).foreach {
    case (testRun, index) =>
      testRun.split("\"\"\"") match {
        case Array(doc, body) =>
          body.trim.split("\\$ ").filterNot(_ == "").map
          {
            case testCase => {
              val Array(argv_, tmp@_*) = testCase.trim.split("\n")
              val expectedResultString = tmp.mkString("\n")
              val Array("prog", argv@_*) = argv_.split(" ")
              try {
                val expectedResult = Json.build(Json.parse(expectedResultString))
                val result = Json.build(Docopt(doc, argv.toArray))
                if (result != expectedResult) {
                  println("===== %d: FAILED =====".format(index))
                  println("\"\"\"" + doc + "\"\"\"")
                  println(argv_)
                  println("result> " + result)
                  println("expected> " + expectedResult + "\n")
                }
              } catch {
                case _:Throwable =>
                  if (expectedResultString != "\"user-error\"") {
                    println("===== %d: BAD JSON =====".format(index))
                    println("\"\"\"" + doc + "\"\"\"")
                    println(argv_)
                    println("result> " + "\"user-error\"")
                    println("expected> " + expectedResultString + "\n")
                  }
              }
            }
          }
        case _ =>
      }
    case _ =>
  }
}
