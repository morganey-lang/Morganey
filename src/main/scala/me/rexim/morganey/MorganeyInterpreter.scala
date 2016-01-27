package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.syntax.LambdaParser

import scala.io.Source
import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  type Context = List[MorganeyBinding]

  def interpretOneNode(node: MorganeyNode, context: Context): (LambdaTerm, Context) = {
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

  def interpertNodes(nodes: Stream[MorganeyNode], initialContext: Context): Stream[(LambdaTerm, Context)] =
    nodes match {
      case firstNode #:: restNodes => {
        lazy val result : Stream[(LambdaTerm, Context)] =
          interpretOneNode(firstNode, initialContext) #:: restNodes.zip(result).map {
            case (node, (_, context)) => interpretOneNode(node, context)
          }

        result
      }

      case _ => Stream.empty
  }

  def interpretFile(fileName: String, initialContext: Context) : Try[Stream[(LambdaTerm, Context)]] = {
    Try(Source.fromFile(fileName).mkString)
      .map (LambdaParser.parseAll(LambdaParser.script, _))
      .flatMap {
        case parsedCode => parsedCode
          .map(Success(_))
          .getOrElse(Failure(new IllegalArgumentException(s"$fileName ${parsedCode.toString}")))
      }
      .map (code => interpertNodes(code.toStream, initialContext))
  }
}
