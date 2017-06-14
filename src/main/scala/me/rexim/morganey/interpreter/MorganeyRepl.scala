package me.rexim.morganey.interpreter

import me.rexim.morganey.Commands
import me.rexim.morganey.ast.error.{BindingLoop, NonExistingBinding}
import me.rexim.morganey.reduction.NormalOrder._
import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyLoading, MorganeyNode}
import me.rexim.morganey.reduction.Computation
import me.rexim.morganey.interpreter.TermOutputHelper._
import me.rexim.morganey.syntax._
import me.rexim.morganey.module._

import scala.util.{Failure, Success}

class MorganeyRepl(preludeModule: Option[Module]) {

  def evalLine(context: ReplContext, line: String): Computation[ReplResult[String]] =
    line.trim match {
      case ""            => Computation(ReplResult(context))
      case Commands(cmd) => cmd(context)
      case input         => parseAndEval(context, line)
    }

  // TODO: don't return any
  private def parseAndEval(context: ReplContext, line: String): Computation[ReplResult[String]] = {
    val parseResult = Computation(LambdaParser.parseAll(LambdaParser.replCommand, line).toTry)

    parseResult flatMap {
      case Success(binding: MorganeyBinding) => {
        Computation(ReplResult(
          // TODO: don't make assumption that adding already existing
          // binding redefines it
          context.addBinding(binding),
          if (!context.contains(binding)) {
            None
          } else {
            // TODO: don't make assumptions about bindings structure
            Some(s"${binding.variable.name} was redefined")
          }
        ))
      }
      case Success(node) => evalNode(context, node) map (_ map smartShowTerm)
      case Failure(e)    => Computation.failed(e)
    }
  }

  private def evalNode(context: ReplContext, node: MorganeyNode): Computation[ReplResult[LambdaTerm]] = {
    node match {
      case MorganeyLoading(Some(modulePath)) => {
        new Module(CanonicalPath(modulePath), preludeModule).load() match {
          case Success(bindings) => Computation(ReplResult(context.addBindings(bindings), None))
          case Failure(e) => Computation.failed(e)
        }
      }

      case MorganeyLoading(None) =>
        Computation.failed(new IllegalArgumentException("Module path was not specified!"))

      case term: LambdaTerm =>
        term.addBindings(context.bindings).right.map { t =>
          t.norReduceComputation().map { resultTerm =>
            ReplResult(context, Some(resultTerm))
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
