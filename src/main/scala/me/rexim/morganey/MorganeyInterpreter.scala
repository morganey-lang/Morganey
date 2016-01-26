package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.syntax.LambdaParser

import scala.io.Source
import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  type Context = List[MorganeyBinding]

  def interpretOneMorganeyNode(node: MorganeyNode, context: Context): (LambdaTerm, Context) = {
    node match {
      case MorganeyBinding(variable, term) => {
        val result = term.addContext(context).normalOrder()
        val binding = MorganeyBinding(variable, result)
        (result, binding :: context)
      }

      case term : LambdaTerm => {
        val result = term.addContext(context).normalOrder()
        (result, context)
      }
    }
  }

  def interpertMorganeyNodes(nodes: Stream[MorganeyNode], context: Context): Stream[(LambdaTerm, Context)] = {
    def result : Stream[(LambdaTerm, Context)] = Stream.cons(
      interpretOneMorganeyNode(nodes.head, context),
      nodes.tail.zip(result.map(_._2)).map {
        case (node, context1) => interpretOneMorganeyNode(node, context1)
      }
    )

    result
  }

  def interpretFile(fileName: String, context: Context) : Try[Stream[(LambdaTerm, Context)]] = {
    Try(Source.fromFile(fileName).mkString)
      .map (LambdaParser.parseAll(LambdaParser.script, _))
      .flatMap {
        case parsedCode => parsedCode
          .map(Success(_))
          .getOrElse(Failure(new IllegalArgumentException(s"$fileName ${parsedCode.toString}")))
      }
      .map (code => interpertMorganeyNodes(code.toStream, context))
  }
}
