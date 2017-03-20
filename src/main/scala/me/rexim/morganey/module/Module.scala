package me.rexim.morganey.module

import me.rexim.morganey.ast._
import me.rexim.morganey.syntax._
import me.rexim.morganey.reader._
import scala.util._

import java.net.URL

class Module(modulePath: ModulePath, classLoader: ClassLoader = Module.getClass.getClassLoader) {
  def canonicalPath: String =
    modulePath.asCanonicalPath.path

  private def nodes: Try[List[MorganeyNode]] = {
    val CanonicalPath(canonicalPath) = modulePath.asCanonicalPath
    val ResourcePath(resourcePath) = modulePath.asResourcePath

    lazy val moduleNotFound = Failure(new ModuleNotFoundException(s"$canonicalPath module was not found"))

    for {
      resourceUrl <- Option(classLoader.getResource(resourcePath)).map(Success(_)).getOrElse(moduleNotFound)
      moduleNodes <- withReader(resourceUrl)(LambdaParser.parseAll(LambdaParser.module, _).toTry)
    } yield moduleNodes
  }

  def bindings: Try[Set[MorganeyBinding]] =
    nodes.map(_.collect {
      case binding: MorganeyBinding => binding
    }.toSet)

  def dependencies: Try[Set[Module]] =
    nodes.map(_.collect {
      case MorganeyLoading(Some(canonicalPath)) => new Module(CanonicalPath(canonicalPath), classLoader)
    }.toSet)
}
