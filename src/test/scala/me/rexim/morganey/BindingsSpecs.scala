package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.ast.MorganeyBinding
import me.rexim.morganey.helpers.TestTerms
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
}
