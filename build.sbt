/*
 * =================================== settings ===================================
 */

lazy val commonSettings = Seq(
  name := "morganey",
  organization := "me.rexim",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  libraryDependencies ++= Seq(
    "jline" % "jline" % "2.12.1",
    "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test"
  ),
  test in assembly := {}
)

lazy val macroSettings = Seq(
  name := "morganey-macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val kernelSettings = Seq(
  name := "morganey-kernel",
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)

lazy val dependencySettings =
  "compile->compile;test->test"

/*
 * ==================================== tasks =====================================
 */

lazy val build = TaskKey[Unit]("build", "Builds morganey and copies all generated files into the target folder.")

build := {
  val (_, coreFile) = packagedArtifact.in(Compile, packageBin).value
  val (_, macroFile) = packagedArtifact.in(macros).in(Compile, packageBin).value
  val (_, kernelFile) = packagedArtifact.in(kernel).in(Compile, packageBin).value

  val targetDir = coreFile.getParentFile()

  IO.copyFile(macroFile, targetDir / macroFile.getName())
  IO.copyFile(kernelFile, targetDir / kernelFile.getName())
}

addCommandAlias("rebuild", ";clean;build")
addCommandAlias("retest", ";rebuild;test")

/*
 * =================================== projects ===================================
 */

lazy val morganey = (project in file("."))
  .settings(commonSettings :_*)
  .settings(
    mainClass := Some("me.rexim.morganey.Main"),
    initialCommands in console := "import me.rexim.morganey.meta._"
  )
  .aggregate(macros, kernel)
  .dependsOn(
    macros % dependencySettings,
    kernel % dependencySettings
  )

lazy val macros = (project in file("macros"))
  .settings(commonSettings :_*)
  .settings(macroSettings :_*)
  .dependsOn(kernel % dependencySettings)

lazy val kernel = (project in file("kernel"))
  .settings(commonSettings :_*)
  .settings(kernelSettings :_*)
