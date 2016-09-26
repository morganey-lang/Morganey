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

lazy val stdLibArtifact = Artifact("stdlib", "jar", "jar")

lazy val packStdLib = TaskKey[File]("packStdLib", "Packs the std-lib into a single jar.")

packStdLib := {
  val rootDir = file(".")
  val (_, coreFile) = packagedArtifact.in(Compile, packageBin).value

  val targetDir = coreFile.getParentFile()

  def filesIn(dir: File): Seq[File] = {
    val (files, dirs) = IO.listFiles(dir).partition(_.isFile)
    files.filter(_.getName endsWith ".mgn") ++ dirs.flatMap(filesIn)
  }

  def relativeTo(base: File, f: File): String =
    base.toURI.relativize(f.toURI).getPath

  val stdLibJar = filesIn(rootDir / "std").map(f => f -> relativeTo(rootDir, f))
  val stdLibJarFile = targetDir / s"${name.value}_2.11-${version.value}-stdlib.jar"

  IO.jar(stdLibJar, stdLibJarFile, new java.util.jar.Manifest())
  stdLibJarFile
}

lazy val build = TaskKey[Unit]("build", "Builds morganey and copies all generated files into the target folder.")

build := {
  val (_, coreFile) = packagedArtifact.in(Compile, packageBin).value
  val (_, macroFile) = packagedArtifact.in(macros).in(Compile, packageBin).value
  val (_, kernelFile) = packagedArtifact.in(kernel).in(Compile, packageBin).value
  val _ = packStdLib.value

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
  .settings(addArtifact(stdLibArtifact, packStdLib).settings:_*)
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
