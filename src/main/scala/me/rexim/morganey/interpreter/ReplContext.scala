package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._

case class ReplContext(bindings: List[MorganeyBinding] = Nil) {
  def addBinding(binding: MorganeyBinding): ReplContext = {
    // TODO(#198): Binding redefinition mechanism for REPL context
    ReplContext(binding :: bindings)
  }

  def addBindings(newBindings: List[MorganeyBinding]): ReplContext =
    ReplContext(bindings ++ newBindings)

  def clear(): ReplContext = ReplContext(List())

  def removeBindings(predicate: MorganeyBinding => Boolean): (ReplContext, List[MorganeyBinding])= {
    val (satisfyF, notSatisfyF) = bindings.partition(predicate)
    (ReplContext(satisfyF), notSatisfyF)
  }
}
