package me.rexim

import me.rexim.helpers.TestTerms
import org.scalatest._

class BetaReductionSpec extends FlatSpec with Matchers with TestTerms {

  "An application" should "be beta-reduced" in {
    val inputTerm = LambdaApp(I(x), I(y))
    BetaReduction.reduction(inputTerm) should be (I(y))
  }
}
