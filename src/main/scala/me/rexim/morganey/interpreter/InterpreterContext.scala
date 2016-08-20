package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder

case class InterpreterContext(bindings: List[MorganeyBinding], moduleFinder: ModuleFinder) {
  def addBinding(binding: MorganeyBinding): InterpreterContext =
    InterpreterContext(binding :: bindings, moduleFinder)
}
