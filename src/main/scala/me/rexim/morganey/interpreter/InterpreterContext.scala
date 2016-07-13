package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder

case class InterpreterContext(val bindings: List[MorganeyBinding], val moduleFinder: ModuleFinder) {
  def addBinding(binding: MorganeyBinding): InterpreterContext =
    InterpreterContext(binding :: bindings, moduleFinder)
}
