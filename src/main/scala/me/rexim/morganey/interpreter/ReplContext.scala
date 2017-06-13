package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module._
import scala.util._

object ReplContext {
  def fromModule(module: Module): Try[ReplContext] =
    module.load().map(ReplContext(_))
}

case class ReplContext(bindings: List[MorganeyBinding] = Nil) {
  // TODO(#360): Improve time asymptotic of the add binding to REPL context operation
  //
  // Right now it's O(N), but it can be improved
  def addBinding(binding: MorganeyBinding): ReplContext = {
    ReplContext(binding :: bindings.filter(_.variable != binding.variable))
  }

  def addBindings(newBindings: List[MorganeyBinding]): ReplContext =
    newBindings.foldLeft(this)(_.addBinding(_))

  def clear(): ReplContext = ReplContext(List())

  def removeBindings(predicate: MorganeyBinding => Boolean): (ReplContext, List[MorganeyBinding])= {
    val (satisfyF, notSatisfyF) = bindings.partition(predicate)
    (ReplContext(satisfyF), notSatisfyF)
  }
}
