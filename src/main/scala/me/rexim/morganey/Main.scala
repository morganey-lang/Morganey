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

  private class TerminalReplAutocompletion(context: () => ReplContext, moduleIndex: ModuleIndex) extends Completer {
    override def complete(buffer: String, cursor: Int, candidates: java.util.List[CharSequence]): Int = {
      val suggestions = ReplAutocompletion.complete(buffer, cursor, context(), moduleIndex)
      for (elem <- suggestions) candidates.add(elem)
      if (candidates.isEmpty) -1 else 0
    }
  }

  def startRepl() = {
    Signal.handle(new Signal("INT"), this)

    windowsRedirectedInputHack()

    val preludeModule = new Module(CanonicalPath("std.prelude"))
    val con = new ConsoleReader()
    var globalContext =
      ReplContext.fromModule(preludeModule) match {
        case Success(context) => context
        case Failure(e) =>
          con.println(e.getMessage)
          ReplContext()
      }

    con.setPrompt("λ> ")
    con.addCompleter(new TerminalReplAutocompletion(() => globalContext, new ModuleIndex))

    def line() = Option(con.readLine()).map(_.trim)

    val running = true
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
    import MorganeyCompiler._
    import me.rexim.morganey.reduction.NormalOrder._

    val preludeModule = Some(new Module(CanonicalPath("std.prelude")))

    val result = new Module(ResourcePath(programFile), preludeModule)
      .load()
      .flatMap(compileProgram(() => Source.stdin.toStream))
      .map(_.norReduce())

    result match {
      case Success(term) => println(TermOutputHelper.smartShowTerm(term))
      case Failure(e) => println(e)
    }
  }

  def main(args: Array[String]) = {
    args.toList match {
      case Nil => startRepl()
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
