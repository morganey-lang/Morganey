package me.rexim.morganey.interpreter

import scala.util._
import me.rexim.morganey.ast._
import me.rexim.morganey.syntax._
import me.rexim.morganey.module._
import me.rexim.morganey.monad._
import me.rexim.morganey.reader._
import java.io.Reader

import me.rexim.morganey.ast.error.{BindingLoop, NonExistingBinding}

object MorganeyCompiler {
  def compileProgram(input: () => Stream[Char])(rawProgram: List[MorganeyBinding]): Try[LambdaTerm] = {
    rawProgram.partition(_.variable.name == "main") match {
      case (List(MorganeyBinding(LambdaVar("main"), program)), bindings) =>
        program.addBindings(MorganeyBinding(LambdaVar("input"), LambdaInput(input)) :: bindings) match {
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
