package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.numbers.ChurchPairConverter._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ChurchPairConverterSpec extends FlatSpec with Matchers with TestTerms {
  "An identity function" should "be converted to None" in {
    convertPair(I(x)) should be (None)
  }

  "A church pair" should "be converted to some pair" in {
    val pair = lfunc("z" , lapp(lapp(lvar("z"), lvar("x")), lvar("y")))
    convertPair(pair) should be (Some((lvar("x"), lvar("y"))))
  }
}
