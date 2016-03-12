package me.rexim.morganey

import me.rexim.morganey.ast.{MorganeyBinding, LambdaFunc, LambdaApp}
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ContextSpecs extends FlatSpec with Matchers with TestTerms {
  "Context" should "be wrapped over the expression" in {
    val inputExpression = I(x)
    val context = Seq(
      MorganeyBinding(z, I(z)),
      MorganeyBinding(y, I(y))
    )
    val expectedExpression =
      LambdaApp(LambdaFunc(z, LambdaApp(LambdaFunc(y, I(x)), I(y))), I(z))

    inputExpression.addContext(context) should be (expectedExpression)
  }
}
