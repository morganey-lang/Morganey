package me.rexim.morganey.interpreter

import scala.util._
import me.rexim.morganey.ast._
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}
import me.rexim.morganey.module._
import me.rexim.morganey.util._
import java.io.Reader

import me.rexim.morganey.ast.error.{BindingLoop, NonExistingBinding}

object MorganeyExecutor {

  def interpretNode(node: MorganeyNode, moduleFinder: ModuleFinder): Try[List[MorganeyBinding]] =
    node match {
      case binding: MorganeyBinding => Success(List(binding))
      case MorganeyLoading(Some(modulePath)) => loadModule(modulePath, moduleFinder)
      case MorganeyLoading(None) => Success(List())
    }

  def loadModuleFromReader(reader: Reader, moduleFinder: ModuleFinder): Try[List[MorganeyBinding]] =
    LambdaParser.parseWith(reader, _.module).flatMap {
      nodes => sequence(nodes.map(interpretNode(_, moduleFinder))).map(_.flatten)
    }

  def loadModule(modulePath: String, moduleFinder: ModuleFinder): Try[List[MorganeyBinding]] = {
    lazy val moduleNotFound = Failure(new ModuleNotFoundException(s"$modulePath module was not found"))

    moduleFinder
      .findModuleFile(modulePath)
      .map(Success(_))
      .getOrElse(moduleNotFound)
      .flatMap(withReader(_)(loadModuleFromReader(_, moduleFinder)))
  }

  def compileProgram(input: Stream[Char])(rawProgram: List[MorganeyBinding]): Try[LambdaTerm] = {
    rawProgram.partition(_.variable.name == "main") match {
      case (List(MorganeyBinding(LambdaVar("main"), program)), bindings) =>
        LambdaApp(program, LambdaInput(input)).addBindings(bindings) match {
          case Right(compiledProgram) => Success(compiledProgram)

          case Left(NonExistingBinding(name)) =>
            Failure(new IllegalArgumentException(s"Non-existing binding: $name!"))

          case Left(BindingLoop(loop)) =>
            Failure(new IllegalArgumentException(
              s"""|Binding loop detected: ${loop.mkString(" -> ")}
                  |Please use Y-combinator if you want recursion
               """.stripMargin))
        }
      case _ => Failure(new IllegalArgumentException("Entry point not found!"))
    }
  }
}
