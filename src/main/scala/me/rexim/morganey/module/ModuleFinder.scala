package me.rexim.morganey.module

import java.io.File

class ModuleFinder(val paths: List[File]) {
  def findModuleFile(modulePath: String): Option[File] = {
    paths.toStream
      .map(new File(_, s"$modulePath.morganey"))
      .find(_.isFile())
  }
}
