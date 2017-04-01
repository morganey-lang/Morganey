package me.rexim.morganey.module

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import java.io._
import java.util.Vector
import java.net.URL
import java.nio.charset.StandardCharsets

class ModuleIndexSpec extends FlatSpec with MockitoSugar with Matchers {
  def mockMorganeyIndexUrl(modulesInIndex: List[String]): URL = {
    val morganeyIndexContent = modulesInIndex.mkString("\n")

    val morganeyIndexUrl = mock[URL]
    when(morganeyIndexUrl.openStream())
      .thenReturn(new ByteArrayInputStream(morganeyIndexContent.getBytes(StandardCharsets.UTF_8)))

    morganeyIndexUrl
  }

  behavior of "Module Index"

  it should "return all modules from the index" in {
    import scala.collection.JavaConverters._

    val moduleContainers = List(List("hello.mgn", "world.mgn"), List("a/foo.mgn", "b/bar.mgn"))
    val expectedModules = moduleContainers.flatMap(identity).map(path => new Module(ResourcePath(path)))
    val mockedMorganeyIndexUrls = moduleContainers.map(mockMorganeyIndexUrl)

    val classLoader = mock[ClassLoader]
    when(classLoader.getResources("morganey-index"))
      .thenReturn(new Vector(mockedMorganeyIndexUrls.asJava).elements())

    val moduleIndex = new ModuleIndex(classLoader)

    moduleIndex.modules().map(_.canonicalPath).sorted should
      be (expectedModules.map(_.canonicalPath).sorted)
  }
}
