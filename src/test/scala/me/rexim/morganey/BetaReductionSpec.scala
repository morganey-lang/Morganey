package me.rexim.morganey

import me.rexim.morganey.ast.LambdaApp
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class BetaReductionSpec extends FlatSpec with Matchers with TestTerms {

  "An application" should "be beta-reduced" in {
    val inputTerm = LambdaApp(I(x), I(y))
    inputTerm.reduce() should be (I(y))
  }
}
