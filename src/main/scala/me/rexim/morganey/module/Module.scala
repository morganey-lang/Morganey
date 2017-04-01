package me.rexim.morganey.module

import me.rexim.morganey.ast._
import me.rexim.morganey.syntax._
import me.rexim.morganey.reader._
import me.rexim.morganey.monad._
import scala.util._

import java.net.URL

class Module(modulePath: ModulePath, classLoader: ClassLoader = Module.getClass.getClassLoader) {
  def canonicalPath: String =
    modulePath.asCanonicalPath.path

  def loadProgram(loadedModules: Set[String] = Set.empty): Try[List[MorganeyBinding]] =
    for {
      interalBindings <- bindings.map(_.toList)
      externalDependencies <- dependencies.map(_.toList)
      externalBindings <- sequence(externalDependencies.map(_.loadProgram(loadedModules + canonicalPath))).map(_.flatten)
    } yield interalBindings ++ externalBindings

  private def nodes: Try[List[MorganeyNode]] = {
    val CanonicalPath(canonicalPath) = modulePath.asCanonicalPath
    val ResourcePath(resourcePath) = modulePath.asResourcePath

    lazy val moduleNotFound = Failure(new ModuleNotFoundException(s"$canonicalPath module was not found"))

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
      case MorganeyLoading(Some(canonicalPath)) => new Module(CanonicalPath(canonicalPath), classLoader)
    }.toSet)
}
