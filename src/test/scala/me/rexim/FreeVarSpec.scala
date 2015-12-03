package me.rexim

import me.rexim.helpers.TestTerms
import org.scalatest._

class FreeVarSpec extends FlatSpec with Matchers with TestTerms {
  "A variable" should "have itself as a free variable" in {
    x.hasFreeVar(x) should be (true)
    x.hasFreeVar(y) should not be (true)
  }

  "A function" should "have the same free variable as its body except its parameter" in {
    val f = LambdaFunc(x, LambdaApp(x, y))
    f.hasFreeVar(x) should not be (true)
    f.hasFreeVar(y) should be (true)
  }

  "An application" should "have the union of sets of free variables for its left and right terms" in {
    val a = LambdaApp(x, y)
    a.hasFreeVar(x) should be (true)
    a.hasFreeVar(y) should be (true)
    a.hasFreeVar(z) should not be (true)
  }
}
