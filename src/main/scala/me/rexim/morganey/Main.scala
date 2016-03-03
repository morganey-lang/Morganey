package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast._
import me.rexim.morganey.syntax.LambdaParser
import sun.misc.{Signal, SignalHandler}

import scala.util.{Failure, Success, Try}
import scala.concurrent._

object Main {

  def startRepl() = {
    val signalHandler = new SignalHandler {
      override def handle(signal: Signal): Unit = {
        if (cancel != null) {
          cancel()
        }
      }

      def setCancel(cancel: () => Unit): Unit = {
        this.cancel = cancel
      }

      var cancel: () => Unit = null
    }

    Signal.handle(new Signal("INT"), signalHandler)

    var globalContext = List[MorganeyBinding]()
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()

      if (line == "exit") {
        System.exit(0)
      }

      val nodeParseResult = LambdaParser.parse(LambdaParser.term, line)

      if (nodeParseResult.successful) {
        import reduction.NormalOrder._
        val term = nodeParseResult.get
        val (computation, cancel) = term.norReduceCancellable()

        signalHandler.setCancel(cancel)
        val result = computation()

        con.println(ReplHelper.smartPrintTerm(result))

//        val evalResult = MorganeyInterpreter.evalOneNode(node)(globalContext)
//        globalContext = evalResult.context
//        evalResult.result.foreach(r => con.println(ReplHelper.smartPrintTerm(r)))
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
