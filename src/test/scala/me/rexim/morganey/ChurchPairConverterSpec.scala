package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.numbers.ChurchPairConverter._
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.ast.LambdaTermHelpers._
import org.scalatest._

class ChurchPairConverterSpec extends FlatSpec with Matchers with TestTerms {
  "An identity function" should "be converted to None" in {
    convertPair(I(x)) should be (None)
  }

  "A church pair" should "be pair converted to some pair" in {
    val pair = lfunc("z" , lapp(lapp(lvar("z"), lvar("x")), lvar("y")))
    convertPair(pair) should be (Some((lvar("x"), lvar("y"))))
  }

  "An identity function" should "be list converted to a list with itself" in {
    convertList(I(x)) should be (List(I(x)))
  }

  "A church list" should "be converted to some list" in {
    val rawTerm = "(λ z . ((z x) (λ z . ((z x) y))))"
    val term = LambdaParser.parse(LambdaParser.term, rawTerm).get
    val expectedList = "khooy".map(c => lvar(c.toString))
  }
}
