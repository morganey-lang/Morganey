package me.rexim.morganey

import me.rexim.morganey.interpreter.InterpreterContext

object Commands {

  final type Command = InterpreterContext => (InterpreterContext, Option[String])

  val commandPattern  = ":([a-zA-Z]+)".r
  val commandWithArgs = ":([a-zA-Z]+) (.*)".r

  private val commands =
    Map[String, String => Command](
      "reset" -> resetBindings,
      "exit" -> exitREPL
    ) withDefault unknownCommand

  def unapply(line: String): Option[Command] =
    (line match {
      case commandPattern(cmd)        => Some((cmd, ""))
      case commandWithArgs(cmd, args) => Some((cmd, args))
      case _                          => None
    }).map {
      case (cmd, args) => commands(cmd)(args)
    }

  private def unknownCommand(command: String)(args: String)(context: InterpreterContext): (InterpreterContext, Option[String]) =
    (context, Some(s"Unknown command '$command'!"))

  private def exitREPL(args: String)(context: InterpreterContext): (InterpreterContext, Option[String]) =
    sys.exit(0)

  private def resetBindings(args: String)(context: InterpreterContext): (InterpreterContext, Option[String]) =
    (context.reset(), Some("Cleared all the bindings"))

}
