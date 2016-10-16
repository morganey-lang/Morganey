package me.rexim.morganey.module

import java.io.File
import java.net.URL

import ModuleFinder._

object ModuleFinder {
  val fileExtension = "mgn"

  def modulePathToRelativeFile(modulePath: String): String =
    modulePath.replace('.', File.separatorChar)

  def relativeFileToLoadPath(relativeFile: String): String =
    relativeFile.replace(File.separatorChar, '.')

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
    val resourcePath = s"${modulePathToRelativeFile(modulePath)}.$fileExtension"
    val resourceUrl = classLoader.getResource(resourcePath)
    Option(resourceUrl)
  }
}
