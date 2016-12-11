package me.rexim.morganey.autocompletion.extractors

import me.rexim.morganey.Commands._

class CommandWithArg(commands: Map[String, Command]) {
  def unapply(line: String): Option[(String, String)] = {
    val potentialCommand = parseCommand(line)
    potentialCommand flatMap { case (p, arg) =>
      commands.values find {
        case StringCommand(_, _)  => false
        case TermCommand(name, _) => name == p
      } map (_.name -> arg)
    }
  }
}
