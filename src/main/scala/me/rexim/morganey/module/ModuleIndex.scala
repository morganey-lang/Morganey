package me.rexim.morganey.module

import java.io.File
import java.net.URL

import scala.io.Source

class ModuleIndex(classLoader: ClassLoader = ModuleIndex.getClass.getClassLoader) {
  def modules(): List[Module] = {
    import scala.collection.JavaConversions._

    classLoader
      .getResources("morganey-index")
      .toSet
      .flatMap((url: URL) => Source.fromInputStream(url.openStream()).getLines().toSet)
      .toList
      .map(resourcePath => new Module(ResourcePath(resourcePath)))
  }
}
