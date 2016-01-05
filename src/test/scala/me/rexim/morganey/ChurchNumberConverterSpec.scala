package me.rexim.morganey

import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ChurchNumberConverterSpec extends FlatSpec with Matchers with TestTerms {
  "An identity function" should "be converted to None" in {
    convertNumber(I(x)) should be (None)
  }

  "A Church number" should "be converted to a regular number" in {
    val zero = lnested(List("f", "x"), lvar("x"))
    val one = lnested(List("f", "x"), lapp(lvar("f"), lvar("x")))
    val two = lnested(List("f", "x"), lapp(lvar("f"), lapp(lvar("f"), lvar("x"))))

    convertNumber(zero) should be (Some(0))
    convertNumber(one) should be (Some(1))
    convertNumber(two) should be (Some(2))
  }
}
