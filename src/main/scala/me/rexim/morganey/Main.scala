package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast._
import me.rexim.morganey.syntax.LambdaParser

import scala.util.{Failure, Success, Try}

object Main {

  def startRepl() = {
    var globalContext = List[MorganeyBinding]()
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()
      val termParseResult = LambdaParser.parse(LambdaParser.replCommand, line)

      if (termParseResult.successful) {
        import reduction.NormalOrder._

        termParseResult.get match {
          case MorganeyBinding(variable, term) => {
            globalContext = MorganeyBinding(variable, term.addContext(globalContext).norReduce()) :: globalContext
            con.println("Bound " + variable.name)
          }

          case term : LambdaTerm => {
            val result = term.addContext(globalContext).norReduce()
            con.println(ReplHelper.smartPrintTerm(result))
          }
        }
      } else {
        con.println(termParseResult.toString)
      }
    }
  }

  def main(args: Array[String]) = {
    if (args.isEmpty) {
      startRepl()
    } else {
      args.toStream.foldLeft[Try[MorganeyEval]](Success(MorganeyEval())) { (evalTry, fileName) =>
        evalTry.flatMap { eval =>
          MorganeyInterpreter.readNodes(fileName).map { nodes =>
            nodes.foldLeft(eval) {
              case (evalAcc, node) =>
                val evalNext = evalAcc.flatMap(MorganeyInterpreter.evalOneNode(node))
                evalNext.result.foreach(term => println(ReplHelper.smartPrintTerm(term)))
                evalNext
            }
          }
        }
      } match {
        case Failure(e) => println(s"[ERROR] ${e.getMessage}")
        case _ =>
      }
    }
  }
}
