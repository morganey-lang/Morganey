package me.rexim.morganey

import java.io.FileReader
import java.io.File

import me.rexim.morganey.ast._
import me.rexim.morganey.reduction.Computation
import me.rexim.morganey.reduction.NormalOrder._
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}
import me.rexim.morganey.module.ModuleFinder

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  type Context = List[MorganeyBinding]

  val moduleFinder = new ModuleFinder(List(new File("./std/")))

  def evalNextNode(previousEval: MorganeyEval, nextNode: MorganeyNode): MorganeyEval = {
    previousEval.flatMap(evalOneNode(nextNode))
  }

  def evalAllNodes(initialEval: MorganeyEval)(nodes: List[MorganeyNode]): MorganeyEval = {
    nodes.foldLeft(initialEval)(evalNextNode)
  }

  def evalFile(fileName: String)(eval: MorganeyEval): Try[MorganeyEval] = {
    readNodes(fileName).map(evalAllNodes(eval))
  }

  def evalOneNode(node: MorganeyNode)(context: Context): MorganeyEval = {
    val computation = evalOneNodeComputation(node)(context)
    Await.result(computation.future, Duration.Inf)
  }

  def loadFile(context: Context)(file: File): Computation[MorganeyEval] = {
    readNodes(file.getAbsolutePath()) match {
      case Success(nodes) => evalNodesComputation(nodes)(context).last
      case Failure(e) => Computation.fromFuture(Future.failed(e))
    }
  }

  def evalOneNodeComputation(node: MorganeyNode)(context: Context): Computation[MorganeyEval] = {
    node match {
      case MorganeyBinding(variable, term) =>
        term.addContext(context).norReduceComputation().map { resultTerm =>
          MorganeyEval(MorganeyBinding(variable, resultTerm) :: context, Some(resultTerm))
        }

      case term: LambdaTerm =>
        term.addContext(context).norReduceComputation().map { resultTerm =>
          MorganeyEval(context, Some(resultTerm))
        }

      case MorganeyLoading(modulePath) => {
        moduleFinder.findModuleFile(modulePath) match {
          case Some(moduleFile) => loadFile(context)(moduleFile)
          case None => Computation.fromFuture(Future.failed(new IllegalArgumentException(s"$modulePath doesn't exist")))
        }
      }
    }
  }

  def evalNodesComputation(nodes: List[MorganeyNode])(context: Context): Stream[Computation[MorganeyEval]] = {
    lazy val computations: Stream[Computation[MorganeyEval]] =
      evalOneNodeComputation(nodes.head)(context) #:: computations.zip(nodes.tail).map {
        case (computation, node) => computation.flatMap(eval => evalOneNodeComputation(node)(eval.context))
      }
    computations
  }

  def evalNodes(nodes: List[MorganeyNode])(context: Context): MorganeyEval =
    nodes.foldLeft(MorganeyEval(context)) {
      (eval, node) => eval.flatMap(evalOneNode(node))
    }

  def readNodes(reader: java.io.Reader): Try[List[MorganeyNode]] =
    Try(LambdaParser.parseAll(LambdaParser.script, reader)).flatMap {
      case parsedCode => parsedCode
        .map(Success(_))
        .getOrElse(Failure(new LambdaParserException(s"${parsedCode.toString}")))
    }

  def readNodes(fileName: String): Try[List[MorganeyNode]] =
    Try(new FileReader(fileName)).flatMap { reader =>
      val nodes = readNodes(reader)
      reader.close()
      nodes
    }
}
