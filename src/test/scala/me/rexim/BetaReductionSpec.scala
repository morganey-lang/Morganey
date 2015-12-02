package me.rexim

import me.rexim.helpers.TestTerms
import org.scalatest._

class BetaReductionSpec extends FlatSpec with Matchers with TestTerms {

  "An application" should "be beta-reduced" in {
    val inputTerm = LambdaApp(I(x), I(y))
    inputTerm.reduce() should be (I(y))
  }
}
