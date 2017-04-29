package me.rexim.morganey.module

import me.rexim.morganey.ast._
import me.rexim.morganey.syntax._
import me.rexim.morganey.reader._
import me.rexim.morganey.monad._
import scala.util._

import java.net.{URL, URLClassLoader}

class Module(
  modulePath: ModulePath,
  preludeModule: Option[Module] = None,
  classLoader: ClassLoader = Module.getClass.getClassLoader
) {
  def canonicalPath: String =
    modulePath.asCanonicalPath.path

  /** Loads all of the bindings within the module graph component this module belongs to.
    *
    * @param loadedModules modules that should not be loaded, because
    * they were already loaded at some point
    * @return bindings within the module graph component this module belongs to
    */
  def load(loadedModules: Set[String] = Set.empty): Try[List[MorganeyBinding]] =
    if (!loadedModules.contains(canonicalPath)) {
      for {
        interalBindings <- bindings.map(_.toList)
        externalDependencies <- dependencies.map(_.toList)
        externalBindings <- sequence(externalDependencies.map(_.load(loadedModules + canonicalPath))).map(_.flatten)
      } yield interalBindings ++ externalBindings
    } else {
      Success(Nil)
    }

  private[module] def classPathUrls: Seq[URL] =
    classLoader match {
      case urlClassLoader: URLClassLoader => urlClassLoader.getURLs
      case _ => Seq()
    }

  private[module] def nodes: Try[List[MorganeyNode]] = {
    val CanonicalPath(canonicalPath) = modulePath.asCanonicalPath
    val ResourcePath(resourcePath) = modulePath.asResourcePath
    val classPathList =
      classPathUrls
        .map(url => s"  $url")
        .mkString("\n")

    lazy val moduleNotFound = Failure(new ModuleNotFoundException(
      s"""|$canonicalPath module was not found.
          |$classPathList""".stripMargin
    ))

    for {
      resourceUrl <- Option(classLoader.getResource(resourcePath)).map(Success(_)).getOrElse(moduleNotFound)
      moduleNodes <- withReader(resourceUrl)(LambdaParser.parseAll(LambdaParser.module, _).toTry)
    } yield moduleNodes
  }

  private[module] def bindings: Try[Set[MorganeyBinding]] =
    nodes.map(_.collect {
      case binding: MorganeyBinding => binding
    }.toSet)

  private[module] def dependencies: Try[Set[Module]] =
    nodes.map(_.collect {
      case MorganeyLoading(Some(canonicalPath)) => new Module(CanonicalPath(canonicalPath), preludeModule, classLoader)
    }.toSet | preludeModule.map(Set(_)).getOrElse(Set.empty))
}
