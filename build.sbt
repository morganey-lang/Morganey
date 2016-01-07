name := "morganey"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

mainClass := Some("me.rexim.morganey.Main")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "jline" % "jline" % "2.6"
)
