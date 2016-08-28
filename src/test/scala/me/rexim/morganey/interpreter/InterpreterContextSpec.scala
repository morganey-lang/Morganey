package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.meta._

import org.scalatest._

class InterpreterContextSpec extends FlatSpec with Matchers with TestTerms {
  "Interpreter context" should "allow add bindings to it" in {
    val binding = MorganeyBinding(x, I(x))
    val context = InterpreterContext(List(), new ModuleFinder(List())).addBinding(binding)
    context.bindings should be (List(binding))
  }

  "Interpreter context" should "clear bindings on reset command" in {
    val bindings = List(MorganeyBinding(m"x", m"\\x.x"))
    val context = InterpreterContext(bindings, new ModuleFinder(List()))
    context.reset().bindings.isEmpty should be (true)
  }
}
