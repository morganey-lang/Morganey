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
      val nodeParseResult = LambdaParser.parse(LambdaParser.replCommand, line)

      if (nodeParseResult.successful) {
        val node = nodeParseResult.get
        val evalResult = MorganeyInterpreter.evalOneNode(node)(globalContext)
        globalContext = evalResult.context
        evalResult.result.foreach(r => con.println(ReplHelper.smartPrintTerm(r)))
      } else {
        con.println(nodeParseResult.toString)
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
