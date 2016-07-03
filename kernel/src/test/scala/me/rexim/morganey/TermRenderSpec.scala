package me.rexim.morganey

import me.rexim.morganey.ast.LambdaApp
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class TermRenderSpec extends FlatSpec with Matchers with TestTerms {
  "A variable" should "be rendered to its name" in {
    x.toString should be ("x")
  }

  "A function" should "be rendered to lambda notation" in {
    I(x).toString should be ("(λ x . x)")
  }

  "An application" should "be rendered to lambda notation" in {
    LambdaApp(x, y).toString should be ("(x y)")
  }
}
