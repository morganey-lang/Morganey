package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.syntax.LambdaParser

import scala.io.Source
import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  def interpretOneMorganeyNode(node: MorganeyNode, context: List[MorganeyBinding]): List[MorganeyBinding] = {
    node match {
      case MorganeyBinding(variable, term) =>
        MorganeyBinding(variable, term.addContext(context).normalOrder()) :: context

      case term : LambdaTerm => {
        val result = term.addContext(context).normalOrder()
        println(ReplHelper.smartPrintTerm(result))
        context
      }
    }
  }

  def interpertMorganeyNodes(nodes: List[MorganeyNode], context: List[MorganeyBinding]): List[MorganeyBinding] = {
    nodes.foldLeft(context) {
      case (acc, node) => interpretOneMorganeyNode(node, acc)
    }
  }

  def interpretFile(fileName: String, context: List[MorganeyBinding]) : Try[List[MorganeyBinding]] = {
    Try(Source.fromFile(fileName).mkString)
      .map (LambdaParser.parseAll(LambdaParser.script, _))
      .flatMap {
      case parsedCode => parsedCode
        .map(Success(_))
        .getOrElse(Failure(new IllegalArgumentException(s"$fileName ${parsedCode.toString}")))
    }
      .map (interpertMorganeyNodes(_, context))

  }
}
