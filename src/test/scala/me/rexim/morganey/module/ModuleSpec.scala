package me.rexim.morganey.module

import me.rexim.morganey.ast._
import me.rexim.morganey.mock._

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import scala.util._

import java.net.{URL, URLClassLoader}

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

    new Module(ResourcePath(resourcePath), None, classLoader).bindings should
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

    // TODO(ece83383-d5d4-490f-a5b8-7f566206685f): Fix classLoader mock bun in ModuleSpec
    //
    // - Replace `mockClassLoaderResource(resourcePath, moduleContent)` -> `classLoader`
    // - Run the unit tests
    //
    // The unit tests should not fail
    new Module(ResourcePath(resourcePath), None, mockClassLoaderResource(resourcePath, moduleContent)).dependencies.map(_.map(_.canonicalPath)) should
      be (Success(Set("foo", "bar.baz")))

    val preludeModule = new Module(CanonicalPath("std.prelude"), None, classLoader)
    new Module(ResourcePath(resourcePath), Some(preludeModule), classLoader).dependencies.map(_.map(_.canonicalPath)) should
      be (Success(Set("foo", "bar.baz", "std.prelude")))
  }

  it should "should print list of class path URLs on ModuleNotFound error" in {
    val urls = Array(
      new URL("https://github.com/"),
      new URL("https://www.google.ru/")
    )
    val urlClassLoader = mock[URLClassLoader]
    when(urlClassLoader.getURLs()).thenReturn(urls)

    new Module(ResourcePath("a/b/c.mgn"), None, urlClassLoader).nodes match {
      case Failure(e) => assert(urls.forall { url =>
        e.getMessage.containsSlice(url.toString)
      }, "Not all classpaths are presented in the ModuleNotFound error message")
      case _ => fail("Successfully loaded non-existing module")
    }
  }

  it should "not load already loaded modules into program" in {
    val resourcePath = "a/b/c.mgn"
    val classLoader = mockClassLoaderResource(resourcePath, "def c := c")

    new Module(ResourcePath(resourcePath), None, classLoader)
      .load(Set(ResourcePath(resourcePath).asCanonicalPath.path)) should be (Success(Nil))
  }

  it should "return class path URLs if the provided class loader is URLClassLoader" in {
    val urls = Array(
      new URL("https://github.com/"),
      new URL("https://www.google.ru/")
    )
    val urlClassLoader = mock[URLClassLoader]
    when(urlClassLoader.getURLs()).thenReturn(urls)

    val module = new Module(CanonicalPath("foo.bar"), None, urlClassLoader)
    module.classPathUrls should be (urls)
  }

  it should "return empty list of URLs if the provided class loader is NOT URLClassLoader" in {
    val classLoader = mock[ClassLoader]
    val module = new Module(CanonicalPath("foo.bar"), None, classLoader)
    module.classPathUrls should be (Seq())
  }
}
