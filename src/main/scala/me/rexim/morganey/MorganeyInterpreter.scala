package me.rexim.morganey

import java.io.FileReader

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}
import me.rexim.morganey.reduction.NormalOrder._

import scala.util.{Failure, Success, Try}
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MorganeyInterpreter {
  type Context = List[MorganeyBinding]

  def evalOneNode(node: MorganeyNode)(context: Context): MorganeyEval = {
    val (computation, _) = evalOneNodeCancellable(node)(context)
    Await.result(computation, Duration.Inf)
  }

  def evalOneNodeCancellable(node: MorganeyNode)(context: Context): (Future[MorganeyEval], () => Unit) = {
    node match {
      case MorganeyBinding(variable, term) =>
        val (reductionResult, cancel) = term.addContext(context).norReduceCancellable()
        val morganeyEval = reductionResult.map { result =>
          val binding = MorganeyBinding(variable, result)
          MorganeyEval(binding :: context, Some(result))
        }
        (morganeyEval, cancel)

      case term : LambdaTerm =>
        val (reductionResult, cancel) = term.addContext(context).norReduceCancellable()
        val morganeyEval = reductionResult.map { term =>
          MorganeyEval(context, Some(term))
        }
        (morganeyEval, cancel)
    }
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
