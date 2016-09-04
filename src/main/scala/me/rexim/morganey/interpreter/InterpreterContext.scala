package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder

case class InterpreterContext(bindings: List[MorganeyBinding], moduleFinder: ModuleFinder) {
  def addBinding(binding: MorganeyBinding): InterpreterContext =
    InterpreterContext(binding :: bindings, moduleFinder)

  def clear(): InterpreterContext = InterpreterContext(List(), moduleFinder)

  def removeBindings(predicate: MorganeyBinding => Boolean): (InterpreterContext, List[MorganeyBinding])= {
    val (satisfyF, notSatisfyF) = bindings.partition(predicate)
    (InterpreterContext(satisfyF, moduleFinder), notSatisfyF)
  }
}
