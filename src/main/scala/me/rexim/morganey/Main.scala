package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.interpreter._
import me.rexim.morganey.module._
import me.rexim.morganey.ast._
import me.rexim.morganey.reduction.Computation
import sun.misc.{Signal, SignalHandler}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import java.io.File

import jline.console.completer.Completer
import me.rexim.morganey.autocompletion.ReplAutocompletion

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

    windowsRedirectedInputHack()

    var globalContext = context

    val running = true
    val con = new ConsoleReader()
    con.setPrompt("λ> ")
    con.addCompleter(new TerminalReplAutocompletion(() => globalContext))

    def line() = Option(con.readLine()).map(_.trim)

    // TODO(b6a7635e-46f3-412e-848b-f770d9d4e709): try to get rid of duplicate code
    awaitComputationResult(MorganeyRepl.evalLine(context, "load std.prelude")) match {
      case Success(ReplResult(newContext, _)) =>
        globalContext = newContext
      case Failure(e) =>
        con.println(e.getMessage)
    }

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

  def executeProgram(programFile: String) = {
    // TODO(d60b7de5-11f1-4d98-a30e-3d1baa0aae3e): Implement prelude
    // mechanism for execution mode

    import MorganeyCompiler._
    import me.rexim.morganey.reduction.NormalOrder._

    val result = new Module(ResourcePath(programFile))
      .load()
      .flatMap(compileProgram(() => Source.stdin.toStream))
      .map(_.norReduce())

    result match {
      case Success(term) => println(TermOutputHelper.smartShowTerm(term))
      case Failure(e) => println(e)
    }
  }

  def main(args: Array[String]) = {
    val context =
      ReplContext(List[MorganeyBinding](), new ModuleIndex())

    args.toList match {
      case Nil => startRepl(context)
      case programFile :: _ => executeProgram(programFile)
    }
  }

  private def windowsRedirectedInputHack() = {
    lazy val isInputRedirected = System.console == null
    lazy val isWindows =
      Option(System.getProperty("os.name"))
        .exists(_.toLowerCase.contains("windows"))

    if (isWindows && isInputRedirected) {
      // Need to disable native console usage in Windows with redirected
      // input, otherwise JLine just hangs when trying to read it.
      System.setProperty("jline.WindowsTerminal.directConsole", "false")
    }
  }
}
