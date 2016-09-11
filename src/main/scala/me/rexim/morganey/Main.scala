package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.interpreter._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.interpreter.TermOutputHelper.smartShowTerm
import me.rexim.morganey.ast._
import me.rexim.morganey.reduction.Computation
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.util._
import sun.misc.{Signal, SignalHandler}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import java.io.File

import scala.io.Source

object Main extends SignalHandler {

  override def handle(signal: Signal): Unit = {
    currentComputation.foreach(_.cancel())
  }

  var currentComputation: Option[Computation[ReplResult[LambdaTerm]]] = None

  def awaitComputationResult(computation: Computation[ReplResult[LambdaTerm]]): Try[ReplResult[LambdaTerm]] = {
    try {
      currentComputation = Some(computation)
      Try(Await.result(computation.future, Duration.Inf))
    } finally {
      currentComputation = None
    }
  }

  def exitRepl() = {
    System.exit(0)
  }

  def handleLine(con: ConsoleReader)(globalContext: ReplContext, line: String): Option[ReplContext] = {
    val nodeParseResult = LambdaParser.parseWith(line, _.replCommand)

    val evaluationResult = nodeParseResult flatMap { node =>
      val computation = MorganeyRepl.evalNode(globalContext, node)
      awaitComputationResult(computation)
    }

    evaluationResult match {
      case Success(ReplResult(context, result)) =>
        result.foreach(t => con.println(smartShowTerm(t)))
        Some(context)
      case Failure(e) =>
        con.println(e.getMessage)
        None
    }
  }

  def startRepl(context: ReplContext) = {
    Signal.handle(new Signal("INT"), this)

    val running = true
    var globalContext = context
    val con = new ConsoleReader()
    con.setPrompt("λ> ")
    con.addCompleter(new ReplAutocompletion(() => globalContext))

    def line() = Option(con.readLine()).map(_.trim)

    val evalLine = handleLine(con) _

    while (running) line() match {
      case None                => exitRepl() // eof
      case Some("")            => ()
      case Some(Commands(cmd)) =>
        val ReplResult(newContext, output) = cmd(globalContext)
        globalContext = newContext
        output.foreach(con.println)
      case Some(line)          => evalLine(globalContext, line) foreach { context =>
        globalContext = context
      }
    }
  }

  def executeProgram(context: ReplContext, programFile: String) = {
    import MorganeyExecutor._
    import me.rexim.morganey.reduction.NormalOrder._

    val result = loadModuleFromReader(new java.io.FileReader(programFile), context.moduleFinder, Set())
      .flatMap(compileProgram(Source.stdin.toStream))
      .map(_.norReduce())

    result match {
      case Success(term) => println(TermOutputHelper.smartShowTerm(term))
      case Failure(e) => println(e)
    }
  }

  def main(args: Array[String]) = {
    val context =
      ReplContext(List[MorganeyBinding](), new ModuleFinder(List(new File("./std/"))))

    args.toList match {
      case Nil => startRepl(context)
      case programFile :: _ => executeProgram(context, programFile)
    }
  }
}
