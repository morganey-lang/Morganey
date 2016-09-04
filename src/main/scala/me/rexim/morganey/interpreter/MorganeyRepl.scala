package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.error.{BindingLoop, NonExistingBinding}
import me.rexim.morganey.reduction.NormalOrder._
import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyLoading, MorganeyNode}
import me.rexim.morganey.reduction.Computation

import scala.util.{Failure, Success}

object MorganeyRepl {
  def evalNode(context: InterpreterContext, node: MorganeyNode): Computation[MorganeyEval] = {
    node match {
      case MorganeyLoading(Some(module)) => {
        MorganeyExecutor.loadModule(module, context.moduleFinder, Set()) match {
          case Success(bindings) => Computation(MorganeyEval(context.addBindings(bindings), None))
          case Failure(e) => Computation.failed(e)
        }
      }

      case MorganeyLoading(None) =>
        Computation.failed(new IllegalArgumentException("Module path was not specified!"))

      case binding: MorganeyBinding =>
        Computation(MorganeyEval(context.addBinding(binding), None))

      case term: LambdaTerm =>
        term.addBindings(context.bindings).right.map { t =>
          t.norReduceComputation().map { resultTerm =>
            MorganeyEval(context, Some(resultTerm))
          }
        } match {
          case Right(result) => result
          case Left(NonExistingBinding(name)) =>
            Computation.failed(new IllegalArgumentException(s"Non-existing binding: $name"))

          case Left(BindingLoop(loop)) =>
            Computation.failed(new IllegalArgumentException(
              s"""|Binding loop detected: ${loop.mkString(" -> ")}
                  |Please use Y-combinator if you want recursion
               """.stripMargin))
        }
    }
  }
}
