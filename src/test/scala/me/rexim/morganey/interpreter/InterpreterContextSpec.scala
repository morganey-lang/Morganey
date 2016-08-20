package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.helpers.TestTerms

import org.scalatest._

class InterpreterContextSpec extends FlatSpec with Matchers with TestTerms {
  "Interpreter context" should "allow add bindings to it" in {
    val binding = MorganeyBinding(x, I(x));
    val context = InterpreterContext(List(), new ModuleFinder(List())).addBinding(binding)
    context.bindings should be (List(binding))
  }
}
