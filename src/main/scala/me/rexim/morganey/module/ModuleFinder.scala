package me.rexim.morganey.module

import java.io.File
import java.net.URL

import ModuleFinder._

import scala.io.Source

object ModuleFinder {
  val fileExtension = "mgn"

  def modulePathToRelativeFile(modulePath: String): String =
    modulePath.replace('.', File.separatorChar)

  def modulePathToRelativeURL(modulePath: String): String =
    modulePath.replace('.', '/')

  def relativeFileToLoadPath(relativeFile: String): String =
    relativeFile.replace(File.separatorChar, '.')

  def validMorganeyElement(f: File) =
    f.isDirectory || isMorganeyModule(f)

  def isMorganeyModule(file: File) =
    file.isFile() && (file.getName endsWith s".$fileExtension")
}

class ModuleFinder(val paths: List[File]) {
  def findModuleFile(modulePath: String): Option[File] = {
    paths.toStream
      .map(new File(_, s"${modulePathToRelativeFile(modulePath)}.$fileExtension"))
      .find(_.isFile())
  }

  def findModuleInClasspath(modulePath: String): Option[URL] = {
    val classLoader = this.getClass.getClassLoader
    val resourcePath = s"${modulePathToRelativeURL(modulePath)}.$fileExtension"
    val resourceUrl = classLoader.getResource(resourcePath)
    Option(resourceUrl)
  }

  def findAllModulesInIndex(): Set[String] = {
    import scala.collection.JavaConversions._

    val classLoader = this.getClass.getClassLoader
    classLoader
      .getResources("morganey-index")
      .toSet
      .flatMap((url: URL) => Source.fromInputStream(url.openStream()).getLines().toSet)
  }

  /**
    * Returns a sequence of all directories and .*mgn-Files at the top level of all module paths
    *
    * Imagine having a two folders in the morganey path `modsA` and `modsB`:
    *
    * `modsA`:
    * +---list.mgn
    * +---pair.mgn
    * |
    * \---math
    *     \---arithmetic.mgn
    *
    * `modsB`:
    * +---logic.mgn
    * +---bools.mgn
    * |
    * \---numbers
    *     +--- floats.mgn
    *     \--- whole-numbers.mgn
    *
    * In that case `topLevelDefinitions` should return
    * {{{
    * Seq(list.mgn, pair.mgn, math, logic.mgn, bools.mgn, numbers)
    * }}}
    */
  def topLevelDefinitions() =
    paths.flatMap(path => Option(path.listFiles).toList.flatten).filter(validMorganeyElement)

  /**
    * Returns a list of tuples (a, b), where
    * a = the root directory of this module-path
    * b = the path of a certain module
    *
    * Requirement: `b` is a sub-path of `a`
    */
  def findAllModulesIn(path: String): List[(File, File)] = paths.toStream
    .map { f => (f, new File(f, modulePathToRelativeFile(path))) }
    .filter { case (_, f) => f.exists() }
    .flatMap { case (root, f) => f.listFiles() map (root -> _) }
    .filter { case (_, f) => validMorganeyElement(f) }
    .distinct
    .toList
}
