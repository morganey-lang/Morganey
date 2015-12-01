package me.rexim

import me.rexim.helpers.TestVariables
import org.scalatest._

class BetaReductionSpec extends FlatSpec with Matchers with TestVariables {
  "A variable" should "be substituted with term" in {
    val variable = x
    val term = LambdaFunc(y, y)
    BetaReduction.substitute(variable, x, term) should be (term)
  }

  it should "not be substituted with term if it's different" in {
    val variable = x
    val term = LambdaFunc(y, y)
    BetaReduction.substitute(variable, z, term) should be (variable)
  }

  "An application" should "be substituted recursively" in {
    val inputTerm = LambdaApp(x, y)
    val expectedTerm = LambdaApp(z, y)

    BetaReduction.substitute(inputTerm, x, z) should be (expectedTerm)
  }

  "A function" should "avoid substitution of its variable" in {
    val inputTerm = LambdaFunc(x, x)
    val expectedTerm = inputTerm

    BetaReduction.substitute(inputTerm, x, z) should be (expectedTerm)
  }

  "A function" should "be substituted" in {
    val inputTerm = LambdaFunc(y, x)
    val expectedTerm = LambdaFunc(y, z)

    BetaReduction.substitute(inputTerm, x, z) should be (expectedTerm)
  }

  "An application" should "be beta-reduced" in {
    val xId = LambdaFunc(x, x)
    val yId = LambdaFunc(y, y)
    val inputTerm = LambdaApp(xId, yId)

    BetaReduction.reduction(inputTerm) should be (yId)
  }
}
