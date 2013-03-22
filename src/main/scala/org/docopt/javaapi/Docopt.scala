package org.docopt.javaapi

import scala.collection.JavaConversions._

object Docopt {
  def apply(usage: String,
            argv: Array[String],
            help: Boolean,
            version: String,
            optionsFirst: Boolean) = mapAsJavaMap(org.docopt.Docopt(usage, argv, help, version, optionsFirst))
}