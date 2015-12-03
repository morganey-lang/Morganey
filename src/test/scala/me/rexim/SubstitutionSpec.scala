package me.rexim

import me.rexim.helpers.TestTerms
import org.scalatest._

class SubstitutionSpec extends FlatSpec with Matchers with TestTerms {
  "A variable" should "be substituted with term" in {
    val term = I(y)
    x.substitute(x -> term) should be (term)
  }

  it should "not be substituted with term if it's different" in {
    val term = I(y)
    x.substitute(z -> term) should be (x)
  }

  "An application" should "be substituted recursively" in {
    val inputTerm = LambdaApp(x, y)
    val expectedTerm = LambdaApp(z, y)

    inputTerm.substitute(x -> z) should be (expectedTerm)
  }

  "A function" should "avoid substitution of its variable" in {
    val inputTerm = I(x)
    val expectedTerm = inputTerm

    inputTerm.substitute(x -> z) should be (expectedTerm)
  }

  "A function" should "be substituted" in {
    val inputTerm = LambdaFunc(y, x)
    val expectedTerm = LambdaFunc(y, z)

    inputTerm.substitute(x -> z) should be (expectedTerm)
  }

  "A function's parameter" should "be alpha-renamed when the " +
    "replacement contains the parameter as the free variable" in {
    val x0 = alphaVar("x", 0)
    val x1 = alphaVar("x", 1)
    val f = LambdaFunc(x, y)
    val g = LambdaFunc(x, LambdaApp(x0, y))

    f.substitute(y -> x) should be (LambdaFunc(x0, x))
    g.substitute(y -> x) should be (LambdaFunc(x1, LambdaApp(x0, x)))
  }
}
