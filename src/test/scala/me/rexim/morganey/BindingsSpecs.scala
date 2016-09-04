package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.ast.MorganeyBinding
import me.rexim.morganey.ast.error.{BindingLoop, NonExistingBinding}
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.meta._
import org.scalatest._

class BindingsSpecs extends FlatSpec with Matchers with TestTerms {
  "Bindings" should "be wrapped over the expression" in {
    val khooy = lvar("khooy")
    val nya = lvar("nya")
    val inputExpression = lapp(I(x), khooy)
    val context = Seq(
      MorganeyBinding(y, I(y)),
      MorganeyBinding(khooy, nya)
    )

    inputExpression.addBindings(context) should be
    (lapp(
      lfunc("y",
        lapp(
          lfunc("khooy", inputExpression),
          nya)),
      I(y)))
  }

  "Dependent bindings" should "produce NonExistingBinding error on non-existing binding" in {
    val bindings = List(
      MorganeyBinding(m"x", m"\\x. x"),
      MorganeyBinding(m"y", m"\\y. y")
    )

    m"a".addDependentBindings(bindings) should be (Left(NonExistingBinding("a")))
  }

  "Dependent bindings" should "produce BindingLoop error on a loop in bindings" in {
    val bindings = List(
      MorganeyBinding(m"a", m"b"),
      MorganeyBinding(m"b", m"a")
    )

    m"a".addDependentBindings(bindings) should be (Left(BindingLoop(List("a", "b", "a"))))
  }
}
