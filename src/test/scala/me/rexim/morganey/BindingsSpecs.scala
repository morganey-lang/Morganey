package me.rexim.morganey

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.error.{BindingLoop, NonExistingBinding}
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.meta._
import org.scalatest._

class BindingsSpecs extends FlatSpec with Matchers with TestTerms {
  "Add bindings" should "should wrap bindings over the expression" in {
    val bindings = List(
      MorganeyBinding(m"a", m"1"),
      MorganeyBinding(m"b", m"a")
    )

    m"b".addBindings(bindings) should be (Right(m"(\\b. b) ((\\a. a) 1)"))
  }

  "Adding bindings" should "produce NonExistingBinding error on non-existing binding" in {
    val bindings = List(
      MorganeyBinding(m"x", m"\\x. x"),
      MorganeyBinding(m"y", m"\\y. y")
    )

    m"a".addBindings(bindings) should be (Left(NonExistingBinding("a")))
  }

  "Adding bindings" should "produce BindingLoop error on a loop in bindings" in {
    val bindings = List(
      MorganeyBinding(m"a", m"b"),
      MorganeyBinding(m"b", m"a")
    )

    m"a".addBindings(bindings) should be (Left(BindingLoop(List("a", "b", "a"))))
  }
}
