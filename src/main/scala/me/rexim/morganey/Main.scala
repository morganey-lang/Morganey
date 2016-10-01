package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.interpreter._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.ast._
import me.rexim.morganey.reduction.Computation
import sun.misc.{Signal, SignalHandler}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import java.io.File

import jline.console.completer.Completer

import scala.io.Source

object Main extends SignalHandler {

  override def handle(signal: Signal): Unit = {
    currentComputation.foreach(_.cancel())
  }

  var currentComputation: Option[Computation[ReplResult[String]]] = None

  def awaitComputationResult(computation: Computation[ReplResult[String]]): Try[ReplResult[String]] = {
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

  private class TerminalReplAutocompletion(context: () => ReplContext) extends Completer {
    override def complete(buffer: String, cursor: Int, candidates: java.util.List[CharSequence]): Int = {
      val suggestions = ReplAutocompletion.complete(buffer, cursor, context())
      for (elem <- suggestions) candidates.add(elem)
      if (candidates.isEmpty) -1 else 0
    }
  }

  def startRepl(context: ReplContext) = {
    Signal.handle(new Signal("INT"), this)

    val running = true
    var globalContext = context
    val con = initializeConsoleReader()
    con.addCompleter(new TerminalReplAutocompletion(() => globalContext))

    def line() = Option(con.readLine()).map(_.trim)

    while (running) line() match {
      case None                => exitRepl() // eof
      case Some(line)          =>
        val computation = MorganeyRepl.evalLine(globalContext, line)
        awaitComputationResult(computation) match {
          case Success(ReplResult(newContext, message)) =>
            globalContext = newContext
            message.foreach(con.println)
          case Failure(e)                               =>
            con.println(e.getMessage)
        }
    }
  }

  def executeProgram(context: ReplContext, programFile: String) = {
    import MorganeyExecutor._
    import me.rexim.morganey.reduction.NormalOrder._

    val result = loadModuleFromReader(new java.io.FileReader(programFile), context.moduleFinder, Set())
      .flatMap(compileProgram(() => Source.stdin.toStream))
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

  private def initializeConsoleReader(): ConsoleReader = {
    lazy val isInputRedirected = System.console == null
    lazy val isWindows =
      Option(System.getProperty("os.name"))
        .exists(_.toLowerCase.contains("windows"))

    if (isWindows && isInputRedirected) {
      // Need to disable native console usage in Windows with redirected
      // input, otherwise JLine just hangs when trying to read it.
      System.setProperty("jline.WindowsTerminal.directConsole", "false")
    }

    val con = new ConsoleReader()
    con.setPrompt("λ> ")
    con
  }
}
