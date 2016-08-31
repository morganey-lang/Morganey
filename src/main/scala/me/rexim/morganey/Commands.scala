package me.rexim.morganey

import me.rexim.morganey.interpreter.InterpreterContext
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.util._

import scala.util.{Failure, Success}

object Commands {

  final type Command = InterpreterContext => (InterpreterContext, Option[String])

  val commandPattern  = ":([a-zA-Z]*)".r
  val commandWithArgs = ":([a-zA-Z]*) (.*)".r

  val commands =
    Map[String, String => Command](
      "exit"  -> exitREPL,
      "raw"   -> rawPrintTerm,
      "reset" -> resetBindings
    ) withDefault unknownCommand

  def parseCommand(line: String): Option[(String, String)] =
    line match {
      case commandPattern(cmd)        => Some((cmd, ""))
      case commandWithArgs(cmd, args) => Some((cmd, args))
      case _                          => None
    }

  def unapply(line: String): Option[Command] =
    parseCommand(line).map {
      case (cmd, args) => commands(cmd)(args)
    }

  private def unknownCommand(command: String)(args: String)(context: InterpreterContext): (InterpreterContext, Option[String]) =
    (context, Some(s"Unknown command '$command'!"))

  private def exitREPL(args: String)(context: InterpreterContext): (InterpreterContext, Option[String]) =
    sys.exit(0)

  private def rawPrintTerm(args: String)(context: InterpreterContext): (InterpreterContext, Option[String]) = {
    val parseResult = LambdaParser.parseWith(args, _.term)
    val output = parseResult match {
      case Success(term) => term.toString
      case Failure(e)    => e.getMessage
    }
    (context, Option(output))
  }

  private def resetBindings(args: String)(context: InterpreterContext): (InterpreterContext, Option[String]) =
    (context.reset(), Some("Cleared all the bindings"))

}
