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
        termParseResult.get match {
          case MorganeyBinding(variable, term) => {
            globalContext = MorganeyBinding(variable, term.addContext(globalContext).normalOrder()) :: globalContext
            con.println("Bound " + variable.name)
          }

          case term : LambdaTerm => {
            val result = term.addContext(globalContext).normalOrder()
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
      args.toStream.foldLeft[Try[List[MorganeyBinding]]](Success(List())) {
        case (contextTry, fileName) => contextTry.flatMap {
          context => {
            MorganeyInterpreter.interpretFile(fileName, context).flatMap {
              result => {
                result.foreach(x => println(ReplHelper.smartPrintTerm(x._1)))
                Success(result.last._2)
              }
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
