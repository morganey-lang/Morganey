package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleIndex

case class ReplContext(bindings: List[MorganeyBinding], moduleIndex: ModuleIndex) {
  def addBinding(binding: MorganeyBinding): ReplContext = {
    // TODO(#198): Binding redefinition mechanism for REPL context
    ReplContext(binding :: bindings, moduleIndex)
  }

  def addBindings(newBindings: List[MorganeyBinding]): ReplContext =
    ReplContext(bindings ++ newBindings, moduleIndex)

  def clear(): ReplContext = ReplContext(List(), moduleIndex)

  def removeBindings(predicate: MorganeyBinding => Boolean): (ReplContext, List[MorganeyBinding])= {
    val (satisfyF, notSatisfyF) = bindings.partition(predicate)
    (ReplContext(satisfyF, moduleIndex), notSatisfyF)
  }
}
