package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder

case class ReplContext(bindings: List[MorganeyBinding], moduleFinder: ModuleFinder) {
  def addBinding(binding: MorganeyBinding): ReplContext =
    ReplContext(binding :: bindings, moduleFinder)

  def addBindings(newBindings: List[MorganeyBinding]): ReplContext =
    ReplContext(bindings ++ newBindings, moduleFinder)

  def clear(): ReplContext = ReplContext(List(), moduleFinder)

  def removeBindings(predicate: MorganeyBinding => Boolean): (ReplContext, List[MorganeyBinding])= {
    val (satisfyF, notSatisfyF) = bindings.partition(predicate)
    (ReplContext(satisfyF, moduleFinder), notSatisfyF)
  }
}
