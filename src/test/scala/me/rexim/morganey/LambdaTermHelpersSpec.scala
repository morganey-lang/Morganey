package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaFunc, LambdaVar}
import me.rexim.morganey.ast.LambdaTermHelpers._
import org.scalatest.{Matchers, FlatSpec}

class LambdaTermHelpersSpec extends FlatSpec with Matchers {
  "func constructor" should "construct functions with multiple arguments as multiple" +
    "nested functions" in {
    lfunc(List("x", "y"), LambdaVar("x")) should be (LambdaFunc(LambdaVar("x"),
      LambdaFunc(LambdaVar("y"), LambdaVar("x"))
    ))
  }
}
