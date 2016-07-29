package me.rexim.morganey.interpreter

import java.io.File

import me.rexim.morganey.ast._
import me.rexim.morganey.reduction.Computation
import me.rexim.morganey.reduction.NormalOrder._
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.util._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  def evalNextNode(previousEval: MorganeyEval, nextNode: MorganeyNode): MorganeyEval = {
    previousEval.flatMap(evalOneNode(nextNode))
  }

  def evalAllNodes(initialEval: MorganeyEval)(nodes: List[MorganeyNode]): MorganeyEval = {
    nodes.foldLeft(initialEval)(evalNextNode)
  }

  def evalFile(fileName: String)(eval: MorganeyEval): Try[MorganeyEval] = {
    readNodes(fileName).map(evalAllNodes(eval))
  }

  def evalOneNode(node: MorganeyNode)(context: InterpreterContext): MorganeyEval = {
    val computation = evalOneNodeComputation(node)(context)
    Await.result(computation.future, Duration.Inf)
  }

  def loadFile(context: InterpreterContext)(file: File): Computation[MorganeyEval] = {
    readNodes(file.getAbsolutePath()) match {
      case Success(nodes) => evalNodesComputation(nodes)(context).last
      case Failure(e) => Computation.fromFuture(Future.failed(e))
    }
  }

  def evalOneNodeComputation(node: MorganeyNode)(context: InterpreterContext): Computation[MorganeyEval] = {
    node match {
      case MorganeyBinding(variable, term) =>
        term.addBindings(context.bindings).norReduceComputation().map { resultTerm =>
          MorganeyEval(context.addBinding(MorganeyBinding(variable, resultTerm)), Some(resultTerm))
        }

      case term: LambdaTerm =>
        term.addBindings(context.bindings).norReduceComputation().map { resultTerm =>
          MorganeyEval(context, Some(resultTerm))
        }

      case MorganeyLoading(optionalModulePath) => {
        optionalModulePath match {
          case Some(modulePath) => context.moduleFinder.findModuleFile(modulePath) match {
            case Some(moduleFile) => loadFile(context)(moduleFile)
            case None => Computation.failed(new IllegalArgumentException(s"$modulePath doesn't exist"))
          }
          case None => Computation.failed(new IllegalArgumentException("Module path was not specified!"))
        }
      }
    }
  }

  def evalNodesComputation(nodes: List[MorganeyNode])(context: InterpreterContext): Stream[Computation[MorganeyEval]] = {
    lazy val computations: Stream[Computation[MorganeyEval]] =
      evalOneNodeComputation(nodes.head)(context) #:: computations.zip(nodes.tail).map {
        case (computation, node) => computation.flatMap(eval => evalOneNodeComputation(node)(eval.context))
      }
    computations
  }

  def evalNodes(nodes: List[MorganeyNode])(context: InterpreterContext): MorganeyEval =
    nodes.foldLeft(MorganeyEval(context)) {
      (eval, node) => eval.flatMap(evalOneNode(node))
    }

  def readNodes(reader: java.io.Reader): Try[List[MorganeyNode]] =
    LambdaParser.parseWith(reader, _.script)

  def readNodes(fileName: String): Try[List[MorganeyNode]] =
    withReader(fileName)(readNodes)
}
