package me.rexim.morganey.module

import org.scalatest._
import org.scalatest.mockito.MockitoSugar
import java.io.File
import java.net.URL
import org.mockito.Mockito._

class ModuleFinderSpec extends FlatSpec with MockitoSugar with Matchers {
  val moduleFinder =
    new ModuleFinder(List(new File("./std/src/main/resources/")))

  "Module finder" should "find high-level modules in the modules path" in {
    moduleFinder.findModuleFile("std.prelude").map(_.getName()) should be (Some("prelude.mgn"))
  }

  it should "find nested modules in the modules path" in {
    moduleFinder.findModuleFile("std.math.arithmetic").map(_.getName()) should be (Some("arithmetic.mgn"))
  }

  it should "not find unexisting modules" in {
    moduleFinder.findModuleFile("khooy") should be (None)
  }

  it should "find modules in JVM classpath" in {
    val expectedUrl = new URL("file://std/prelude.mgn")
    val classLoader = mock[ClassLoader]
    when(classLoader.getResource("std/prelude.mgn")).thenReturn(expectedUrl)
    val moduleFinder = new ModuleFinder(Nil, classLoader)

    moduleFinder.findModuleInClasspath("std.prelude") should
      be (Some(expectedUrl))
  }
}
