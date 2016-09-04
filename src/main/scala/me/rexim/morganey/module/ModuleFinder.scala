package me.rexim.morganey.module

import java.io.File

import ModuleFinder._

object ModuleFinder {
  val fileExtension = "mgn"

  def loadPathToRelativeFile(modulePath: String): String =
    modulePath.replace('.', File.separatorChar)

  def relativeFileToLoadPath(relativeFile: String): String =
    relativeFile.replace(File.separatorChar, '.')

  def isMorganeyModule(file: File) =
    file.isFile() && (file.getName endsWith s".$fileExtension")
}

class ModuleFinder(val paths: List[File]) {
  def findModuleFile(modulePath: String): Option[File] = {
    paths.toStream
      .map(new File(_, s"${loadPathToRelativeFile(modulePath)}.$fileExtension"))
      .find(_.isFile())
  }
}
