package me.rexim.morganey.module

import org.scalatest._
import java.io.File

class ModuleFinderSpec extends FlatSpec with Matchers {
  val moduleFinder =
    new ModuleFinder(List(new File("./std/")))

  "Module finder" should "find modules in the modules path" in {
    moduleFinder.findModuleFile("arithmetic").map(_.getName()) should be (Some("arithmetic.morganey"))
  }

  "Module finder" should "not find unexisting modules" in {
    moduleFinder.findModuleFile("khooy") should be (None)
  }

}
