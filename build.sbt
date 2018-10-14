organization := "org.docopt"
name := "docopt"
version := "0.1-SNAPSHOT"

scalaVersion := "2.10.7"

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.7")

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

