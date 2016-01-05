package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.ChurchNumberHelpers._
import me.rexim.morganey.church.ChurchPairHelpers._
import me.rexim.morganey.church.ChurchPairConverter._
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.syntax.LambdaParser
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

  "A church list of number" should "be converted to some list of numbers" in {
    val rawTerm = "(λ z . ((z (\\f . (\\x . (f (f x))))) (λ z . ((z (\\f . (\\x . (f x)))) (\\f . (\\x . x))))))"
    val term = LambdaParser.parse(LambdaParser.term, rawTerm).get
    val expectedList = Some(List(2, 1, 0))

    convertListOfNumbers(term) should be (expectedList)
  }

  "A church list of ASCII codes" should "be converted to a string" in {
    val one = succ(zero)
    val three = succ(plus(one, one))
    val seven = succ(plus(three, three))
    val nine = mult(three, three)
    val ten = succ(nine)

    val ninetySeven = plus(mult(nine, ten), seven)
    val ninetyEight = succ(ninetySeven)
    val ninetyNine = succ(ninetyEight)

    val abc = pair(ninetySeven, pair(ninetyEight, ninetyNine))

    convertString(abc) should be (Some("abc"))
  }
}
