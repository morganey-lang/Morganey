package me.rexim.morganey.module

import org.scalatest._
import java.io.File

class ModuleFinderSpec extends FlatSpec with Matchers {
  val moduleFinder =
    new ModuleFinder(List(new File("./std/src/main/resources/std/")))

  "Module finder" should "find high-level modules in the modules path" in {
    moduleFinder.findModuleFile("prelude").map(_.getName()) should be (Some("prelude.mgn"))
  }

  "Module finder" should "find nested modules in the modules path" in {
    moduleFinder.findModuleFile("math.arithmetic").map(_.getName()) should be (Some("arithmetic.mgn"))
  }

  "Module finder" should "not find unexisting modules" in {
    moduleFinder.findModuleFile("khooy") should be (None)
  }

  "Module finder" should "find modules in JVM classpath" in {
    val url = moduleFinder.findModuleInClasspath("std.prelude")
    assert(url.isDefined, "std.prelude was not found in classpath")
  }
}
