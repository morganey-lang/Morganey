package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaApp, LambdaFunc}
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class FreeVarSpec extends FlatSpec with Matchers with TestTerms {
  "A variable" should "have itself as a free variable" in {
    x.freeVars.contains("x") should be (true)
    x.freeVars.contains("y") should not be (true)
  }

  "A function" should "have the same free variable as" +
    " its body except its parameter" in {
    val f = LambdaFunc(x, LambdaApp(x, y))
    f.freeVars.contains("x") should not be (true)
    f.freeVars.contains("y") should be (true)
  }

  "An application" should "have the union of sets of free " +
    "variables for its left and right terms" in {
    val a = LambdaApp(x, y)
    a.freeVars.contains("x") should be (true)
    a.freeVars.contains("y") should be (true)
    a.freeVars.contains("z") should not be (true)
  }
}
