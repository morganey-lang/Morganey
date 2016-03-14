package me.rexim.morganey

import java.io.FileReader

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.computation.Computation
import me.rexim.morganey.strategy.{NormalOrder, StrategyComputation}
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  type Context = List[MorganeyBinding]

  def evalOneNode(node: MorganeyNode)(context: Context): MorganeyEval = {
    val computation = evalOneNodeComputation(node)(context)
    Await.result(computation.future, Duration.Inf)
  }

  def evalOneNodeComputation(node: MorganeyNode)(context: Context): Computation[MorganeyEval] = {
    node match {
      case MorganeyBinding(variable, term) =>
        val termWithContext = term.addContext(context)
        val normalOrderComputation = new StrategyComputation(termWithContext, NormalOrder)
        normalOrderComputation.map { resultTerm =>
          MorganeyEval(MorganeyBinding(variable, resultTerm) :: context, Some(resultTerm))
        }

      case term: LambdaTerm =>
        val termWithContext = term.addContext(context)
        val normalOrderComputation = new StrategyComputation(termWithContext, NormalOrder)
        normalOrderComputation.map { resultTerm =>
          MorganeyEval(context, Some(resultTerm))
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
