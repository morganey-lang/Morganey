package me.rexim.morganey

import me.rexim.morganey.ast.{LambdaApp, LambdaFunc}
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class NormalOrderSpec extends FlatSpec with Matchers with TestTerms {
  "A variable" should "be reduced to itself" in {
    x.normalOrder() should be (x)
  }

  "A function" should "be reduced by reducing its body" in {
    LambdaFunc(x, LambdaApp(I(x), y)).normalOrder() should be (LambdaFunc(x, y))
  }
}
