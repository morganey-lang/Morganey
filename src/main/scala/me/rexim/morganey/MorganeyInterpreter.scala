package me.rexim.morganey

import java.io.FileReader

import me.rexim.morganey.ast.{LambdaTerm, MorganeyBinding, MorganeyNode}
import me.rexim.morganey.syntax.{LambdaParserException, LambdaParser}

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

  def interpretNodes(nodes: Stream[MorganeyNode], initialContext: Context): Stream[EvalResult] =
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

  def interpretReader(reader: java.io.Reader, initialContext: Context): Try[Stream[EvalResult]] = {
    Try(LambdaParser.parseAll(LambdaParser.script, reader))
      .flatMap {
        case parsedCode => parsedCode
          .map(Success(_))
          .getOrElse(Failure(new LambdaParserException(s"${parsedCode.toString}")))
      }
      .map(code => interpretNodes(code.toStream, initialContext))
  }

  def interpretFile(fileName: String, initialContext: Context) : Try[Stream[EvalResult]] = {
    Try(new FileReader(fileName))
      .flatMap(reader => interpretReader(reader, initialContext))
      .recoverWith {
        case parserException: LambdaParserException => {
          val message = s"$fileName: ${parserException.getMessage}"
          Failure(new LambdaParserException(message, parserException))
        }
        case e: Throwable => Failure(e)
      }
    }
}
