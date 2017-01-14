package me.rexim.morganey.module

class Module(resourcePath: String) {
  def name: String =
    resourcePath.replace('/', '.').stripSuffix(s".${ModuleFinder.fileExtension}")
}
