package me.rexim.morganey

import me.rexim.morganey.ast.LambdaApp
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class BetaReductionSpec extends FlatSpec with Matchers with TestTerms {

  "An application" should "be beta-reduced if the left term " +
    "is reducible to a function" in {
    val inputTerm = LambdaApp(I(x), I(y))
    inputTerm.reduce() should be (I(y))
  }

  "An application" should "not be beta-reduced if the left term " +
    "is not reducible to a function" in {
    val inputTerm = LambdaApp(x, I(y))
    inputTerm.reduce() should be (inputTerm)
  }
}
