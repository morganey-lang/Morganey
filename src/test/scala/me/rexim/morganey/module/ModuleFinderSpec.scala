package me.rexim.morganey.module

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar


import java.io._
import java.util.Vector
import java.net.URL
import java.nio.charset.StandardCharsets

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

  def mockMorganeyIndexUrl(modulesInIndex: List[String]): URL = {
    val morganeyIndexContent = modulesInIndex.mkString("\n")

    val morganeyIndexUrl = mock[URL]
    when(morganeyIndexUrl.openStream())
      .thenReturn(new ByteArrayInputStream(morganeyIndexContent.getBytes(StandardCharsets.UTF_8)))

    morganeyIndexUrl
  }

  it should "return all modules from Morganey index" in {
    import scala.collection.JavaConverters._

    val moduleContainers = List(List("hello.mgn", "world.mgn"), List("a/foo.mgn", "b/bar.mgn"))
    val expectedModules = moduleContainers.flatMap(identity).toSet
    val mockedMorganeyIndexUrls = moduleContainers.map(mockMorganeyIndexUrl)

    val classLoader = mock[ClassLoader]
    when(classLoader.getResources("morganey-index"))
      .thenReturn(new Vector(mockedMorganeyIndexUrls.asJava).elements())

    val moduleFinder = new ModuleFinder(Nil, classLoader)

    moduleFinder.findAllModulesInIndex() should be (expectedModules)
  }
}
