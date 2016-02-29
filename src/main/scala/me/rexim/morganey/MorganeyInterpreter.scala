package me.rexim.morganey

import java.io.FileReader

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}
import me.rexim.morganey.reduction.NormalOrder._

import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  type Context = List[MorganeyBinding]

  def evalOneNode(node: MorganeyNode)(context: Context): MorganeyEval =
    node match {
      case MorganeyBinding(variable, term) =>
        val result = term.addContext(context).norReduce()
        val binding = MorganeyBinding(variable, result)
        MorganeyEval(binding :: context, Some(result))

      case term : LambdaTerm =>
        val result = term.addContext(context).norReduce()
        MorganeyEval(context, Some(result))
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
