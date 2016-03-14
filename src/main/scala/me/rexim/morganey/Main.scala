package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.MorganeyInterpreter.{evalOneNodeComputation, evalOneNode, readNodes}
import me.rexim.morganey.ReplHelper.smartPrintTerm
import me.rexim.morganey.ast._
import me.rexim.morganey.computation.Computation
import me.rexim.morganey.syntax.LambdaParser
import sun.misc.{Signal, SignalHandler}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object Main extends SignalHandler {

  override def handle(signal: Signal): Unit = {
    currentComputation.foreach(_.cancel())
  }

  var currentComputation: Option[Computation[MorganeyEval]] = None

  def awaitComputationResult(computation: Computation[MorganeyEval]): Try[MorganeyEval] = {
    try {
      currentComputation = Some(computation)
      Try(Await.result(computation.future, Duration.Inf))
    } finally {
      currentComputation = None
    }
  }

  def startRepl() = {
    Signal.handle(new Signal("INT"), this)

    var globalContext = List[MorganeyBinding]()
    val con = new ConsoleReader()
    con.setPrompt("λ> ")

    while (true) {
      val line = con.readLine().trim()

      if (line == "exit") {
        System.exit(0)
      }

      if (!line.isEmpty) {
        val nodeParseResult = LambdaParser.parse(LambdaParser.replCommand, line)

        if (nodeParseResult.successful) {
          val node = nodeParseResult.get
          val computation = evalOneNodeComputation(node)(globalContext)

          awaitComputationResult(computation) match {
            case Success(MorganeyEval(context, result)) =>
              globalContext = context
              result.foreach(t => con.println(smartPrintTerm(t)))
            case Failure(e) =>
              con.println(e.getMessage)
          }
        } else {
          con.println(nodeParseResult.toString)
        }
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
