package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.syntax.LambdaParser

import scala.io.Source
import scala.util.{Failure, Success, Try}

object MorganeyInterpreter {
  type Context = List[MorganeyBinding]
  type EvalResult = (LambdaTerm, Context)

  def interpretOneNode(node: MorganeyNode, context: Context): EvalResult = {
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

  def interpertNodes(nodes: Stream[MorganeyNode], initialContext: Context): Stream[EvalResult] =
    nodes match {
      case firstNode #:: restNodes => {
        lazy val result : Stream[EvalResult] =
          interpretOneNode(firstNode, initialContext) #:: restNodes.zip(result).map {
            case (node, (_, context)) => interpretOneNode(node, context)
          }

        result
      }

      case _ => Stream.empty
  }

  def interpretFile(fileName: String, initialContext: Context) : Try[Stream[EvalResult]] = {
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
