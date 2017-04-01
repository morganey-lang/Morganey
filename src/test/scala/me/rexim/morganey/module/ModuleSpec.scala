package me.rexim.morganey.module

import me.rexim.morganey.ast._
import me.rexim.morganey.mock._

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import scala.util._

class ModuleSpec extends FlatSpec with Matchers with ClassLoaderMocking {
  behavior of "Module"

  it should "return its canonical path" in {
    new Module(ResourcePath("a/b/c.mgn")).canonicalPath should be ("a.b.c")
  }

  it should "parse and return bindings inside of the resource it encapsulates" in {
    val moduleContent = """|def a := a
                           |def b := b""".stripMargin
    val resourcePath = "a/b/c.mgn"

    val classLoader: ClassLoader = mockClassLoaderResource(resourcePath, moduleContent)

    new Module(ResourcePath(resourcePath), classLoader).bindings should
      be (Success(Set(
        MorganeyBinding(LambdaVar("a"), LambdaVar("a")),
        MorganeyBinding(LambdaVar("b"), LambdaVar("b"))
      )))
  }

  it should "parse and return module dependencies" in {
    val moduleContent = """|load foo
                           |load bar.baz
                           |def c := c""".stripMargin
    val resourcePath = "a/b/c.mgn"

    val classLoader = mockClassLoaderResource(resourcePath, moduleContent)

    new Module(ResourcePath(resourcePath), classLoader).dependencies.map(_.map(_.canonicalPath)) should
      be (Success(Set("foo", "bar.baz")))
  }

  it should "not load already loaded modules into program" in {
    val resourcePath = "a/b/c.mgn"
    val classLoader = mockClassLoaderResource(resourcePath, "def c := c")

    new Module(ResourcePath(resourcePath), classLoader)
      .loadProgram(Set(ResourcePath(resourcePath).asCanonicalPath.path)) should be (Success(Nil))
  }
}
