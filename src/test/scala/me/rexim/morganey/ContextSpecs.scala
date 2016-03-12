package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.ast.MorganeyBinding
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ContextSpecs extends FlatSpec with Matchers with TestTerms {
  "Context" should "be wrapped over the expression with all the unused bindings filtered out" in {
    val khooy = lvar("khooy")
    val nya = lvar("nya")
    val inputExpression = lapp(I(x), khooy)
    val context = Seq(
      MorganeyBinding(x, I(x)),
      MorganeyBinding(y, I(y)),
      MorganeyBinding(z, I(z)),
      MorganeyBinding(khooy, nya)
    )

    inputExpression.addContext(context) should be (lapp(lfunc("khooy", inputExpression), nya))
  }
}
