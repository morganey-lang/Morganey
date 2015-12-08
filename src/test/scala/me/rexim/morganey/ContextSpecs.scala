package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaFunc, LambdaApp}
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ContextSpecs extends FlatSpec with Matchers with TestTerms {
  "A context" should "be wrapped over the expression" in {
    val inputExpression = I(x)
    val context = Map(
      y -> I(y),
      z -> I(z)
    )
    val expectedExpression =
      LambdaApp(LambdaFunc(z, LambdaApp(LambdaFunc(y, I(x)), I(y))), I(z))

    inputExpression.addContext(context) should be (expectedExpression)
  }
}
