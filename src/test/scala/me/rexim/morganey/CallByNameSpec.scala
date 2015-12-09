package me.rexim.morganey

import me.rexim.morganey.ast.LambdaApp
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class CallByNameSpec extends FlatSpec with Matchers with TestTerms {

  "An application" should "be beta-reduced if the left term " +
    "is reducible to a function" in {
    val inputTerm = LambdaApp(I(x), I(y))
    inputTerm.callByName() should be (I(y))
  }

  "An application" should "not be beta-reduced if the left term " +
    "is not reducible to a function" in {
    val inputTerm = LambdaApp(x, I(y))
    inputTerm.callByName() should be (inputTerm)
  }

  "A sequence of I's" should "always be reduced to the argument" in {
    val input = LambdaApp(I(x), LambdaApp(I(x), LambdaApp(I(x), y)))
    input.callByName() should be (y)
  }
}
