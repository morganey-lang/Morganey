package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module._
import scala.util._

object ReplContext {
  def fromModule(module: Module): Try[ReplContext] =
    module.load().map(ReplContext(_))
}

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
