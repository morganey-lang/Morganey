package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.MorganeyInterpreter.{evalOneNode, readNodes}
import me.rexim.morganey.ReplHelper.smartPrintTerm
import me.rexim.morganey.ast._
import me.rexim.morganey.reduction.ComputationCancelledException
import me.rexim.morganey.syntax.LambdaParser
import sun.misc.{Signal, SignalHandler}

import scala.util.{Failure, Success, Try}
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends SignalHandler {

  override def handle(signal: Signal): Unit = {
    currentCancel.foreach(_())
  }

  var currentCancel: Option[() => Unit] = None

  def withCancel(cancel: () => Unit)(body: => Unit) = {
    try {
      currentCancel = Some(cancel)
      body
    } finally {
      currentCancel = None
    }
  }

  def startRepl() = {
    Signal.handle(new Signal("INT"), this)

    var globalContext = List[MorganeyBinding]()
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()

      if (line == "exit") {
        System.exit(0)
      }

      val nodeParseResult = LambdaParser.parse(LambdaParser.replCommand, line)

      if (nodeParseResult.successful) {
        val node = nodeParseResult.get
        val (evalResult, cancel) = MorganeyInterpreter.evalOneNodeCancellable(node)(globalContext)

        withCancel(cancel) {
          try {
            val MorganeyEval(context, result) = Await.result(evalResult, Duration.Inf)
            globalContext = context
            result.foreach(t => con.println(smartPrintTerm(t)))
          } catch {
            case e: ComputationCancelledException => con.println("Computation cancelled")
          }
        }
      } else {
        con.println(nodeParseResult.toString)
      }
    }
  }

  def evalAndPrintNextNode(previousEval: MorganeyEval, nextNode: MorganeyNode): MorganeyEval = {
    val nextEval = previousEval.flatMap(evalOneNode(nextNode))
    nextEval.result.foreach(term => println(smartPrintTerm(term)))
    nextEval
  }

  def evalAllNodes(initialEval: MorganeyEval)(nodes: List[MorganeyNode]): MorganeyEval = {
    nodes.foldLeft(initialEval)(evalAndPrintNextNode)
  }

  def evalFile(fileName: String)(eval: MorganeyEval): Try[MorganeyEval] = {
    readNodes(fileName).map(evalAllNodes(eval))
  }

  def main(args: Array[String]) = {
    if (args.isEmpty) {
      startRepl()
    } else {
      args.toStream.foldLeft[Try[MorganeyEval]](Success(MorganeyEval())) { (evalTry, fileName) =>
        evalTry.flatMap(evalFile(fileName))
      } match {
        case Failure(e) => println(s"[ERROR] ${e.getMessage}")
        case _ =>
      }
    }
  }
}
