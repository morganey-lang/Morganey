package me.rexim.morganey

import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class FreeVarSpec extends FlatSpec with Matchers with TestTerms {
  "A variable" should "have itself as a free variable" in {
    x.containsFreeVar(x) should be (true)
    x.containsFreeVar(y) should not be (true)
  }

  "A function" should "have the same free variable as" +
    " its body except its parameter" in {
    val f = LambdaFunc(x, LambdaApp(x, y))
    f.containsFreeVar(x) should not be (true)
    f.containsFreeVar(y) should be (true)
  }

  "An application" should "have the union of sets of free " +
    "variables for its left and right terms" in {
    val a = LambdaApp(x, y)
    a.containsFreeVar(x) should be (true)
    a.containsFreeVar(y) should be (true)
    a.containsFreeVar(z) should not be (true)
  }
}
