package me.rexim.morganey.autocompletion.extractors

import me.rexim.morganey.Commands._

object SimpleCommand {
  def unapply(line: String): Option[String] = parseCommand(line).map(_._1)
}
