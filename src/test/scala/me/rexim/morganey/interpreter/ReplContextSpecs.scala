package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.meta._

import org.scalatest._

class ReplContextSpecs extends FlatSpec with Matchers with TestTerms {
  "REPL context" should "allow add bindings to it" in {
    val binding = MorganeyBinding(x, I(x))
    val context = ReplContext(List(), new ModuleFinder()).addBinding(binding)
    context.bindings should be (List(binding))
  }

  "REPL context" should "clear bindings on reset command" in {
    val bindings = List(MorganeyBinding(m"x", m"\\x.x"))
    val context = ReplContext(bindings, new ModuleFinder())
    context.clear().bindings.isEmpty should be (true)
  }

  it should "partition all known bindings" in {
    val zero  = MorganeyBinding(m"zero",  m"0")
    val one   = MorganeyBinding(m"one",   m"1")
    val two   = MorganeyBinding(m"two",   m"2")
    val three = MorganeyBinding(m"three", m"3")

    val knownBindings = List(zero, one, two, three)
    val context = ReplContext(knownBindings, new ModuleFinder())

    val (satisfyCtx, notSatisfy) = context.removeBindings(_.variable.name endsWith "o")
    val ReplContext(satisfy, _) = satisfyCtx
    satisfy    should be (List(zero, two))
    notSatisfy should be (List(one, three))
  }
}
