/*
 * =================================== settings ===================================
 */

lazy val commonSettings = Seq(
  name := "morganey",
  organization := "me.rexim",
  version := "0.1.0",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  resolvers += Resolver.bintrayRepo("keddelzz", "maven"),
  libraryDependencies ++= Seq(
    "jline" % "jline" % "2.12.1",
    "com.github.keddelzz.hidden-args" %% "hidden-args" % "0.0.1",
    "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test",
    "org.mockito" % "mockito-core" % "2.2.28" % "test"
  ),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  test in assembly := {},
  buildInfoKeys := Seq[BuildInfoKey](version),
  buildInfoPackage := "me.rexim.morganey",
  bintrayOrganization := Some("morganey-lang"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
)

lazy val macroSettings = Seq(
  name := "morganey-macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val kernelSettings = Seq(
  name := "morganey-kernel",
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)

lazy val stdlibSettings = Seq(
  name := "morganey-std"
)

lazy val funtestsSettings = Seq(
  name := "morganey-funtests"
)

lazy val dependencySettings =
  "compile->compile;test->test"

/*
 * ==================================== tasks =====================================
 */

lazy val build = TaskKey[Unit]("build", "Builds morganey and copies all generated files into the target folder.")

build := {
  val (_, coreFile)   = packagedArtifact.in(Compile, packageBin).value
  val (_, macroFile)  = packagedArtifact.in(macros).in(Compile, packageBin).value
  val (_, kernelFile) = packagedArtifact.in(kernel).in(Compile, packageBin).value
  val (_, stdlibFile) = packagedArtifact.in(stdlib).in(Compile, packageBin).value

  val targetDir = coreFile.getParentFile()

  IO.copyFile(macroFile, targetDir / macroFile.getName())
  IO.copyFile(kernelFile, targetDir / kernelFile.getName())
  IO.copyFile(stdlibFile, targetDir / stdlibFile.getName())
}


lazy val moduleIndexFileName = "morganey-index"

lazy val buildModuleIndex = TaskKey[Seq[File]]("buildModuleIndex", "Builds the module index in the std-lib subproject.")

buildModuleIndex := {
  val indexFile   = (resourceManaged in stdlib in Compile).value / moduleIndexFileName
  val stdLibRoot  = (resourceDirectory in stdlib in Compile).value
  val moduleIndex = makeModuleIndex(stdLibRoot)
  IO.write(indexFile, moduleIndex)
  Seq(indexFile)
}

def makeModuleIndex(stdLibRoot: File): String = {
  def recurse(file: File): Seq[File] =
    if (file.isDirectory) file.listFiles() match {
      case null => Seq()
      case xs   => xs flatMap recurse
    } else if (file.isFile && (file.getName endsWith ".mgn")) {
      Seq(file)
    } else Seq()

  val stdLibFiles   = recurse(stdLibRoot / "std")
  val relativeFiles = stdLibFiles pair relativeTo(stdLibRoot) map (_._2) map (_.replaceAll("\\\\", "/"))
  relativeFiles.mkString("\n")
}

resourceGenerators in stdlib in Compile += buildModuleIndex.taskValue


addCommandAlias("funtests", ";assembly;funtests/test")
addCommandAlias("rebuild", ";clean;build")
addCommandAlias("retest", ";rebuild;test")

/*
 * =================================== projects ===================================
 */

lazy val morganey = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings :_*)
  .settings(
    mainClass := Some("me.rexim.morganey.Main"),
    initialCommands in console := "import me.rexim.morganey.meta._"
  )
  .aggregate(macros, kernel, stdlib)
  .dependsOn(
    macros % dependencySettings,
    kernel % dependencySettings,
    stdlib % dependencySettings
  )

lazy val macros = (project in file("macros"))
  .settings(commonSettings :_*)
  .settings(macroSettings :_*)
  .dependsOn(
    kernel % dependencySettings
  )

lazy val kernel = (project in file("kernel"))
  .settings(commonSettings :_*)
  .settings(kernelSettings :_*)

lazy val stdlib = (project in file("std"))
  .settings(commonSettings :_*)
  .settings(stdlibSettings :_*)

lazy val funtests = (project in file("funtests"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings :_*)
  .settings(funtestsSettings :_*)
