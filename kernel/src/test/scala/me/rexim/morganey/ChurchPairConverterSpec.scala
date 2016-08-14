package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.ChurchNumberConverter.encodeNumber
import me.rexim.morganey.church.ChurchPairConverter._
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.syntax.LambdaParser
import org.scalatest._

class ChurchPairConverterSpec extends FlatSpec with Matchers with TestTerms {
  "An identity function" should "be converted to None" in {
    decodePair(I(x)) should be (None)
  }

  "A church pair" should "be pair converted to some pair" in {
    val pair = lfunc("z" , lapp(lapp(lvar("z"), lvar("x")), lvar("y")))
    decodePair(pair) should be (Some((lvar("x"), lvar("y"))))
  }

  "An identity function" should "not be recognized as a list" in {
    decodeList(I(x)) should be (None)
  }

  "A church list" should "be converted to some list" in {
    val inputTerm = pair(x, pair(x, pair(y, zero)))
    val expectedList = Some("xxy".map(c => lvar(c.toString)).toList)
    decodeList(inputTerm) should be (expectedList)
  }

  "A church list of number" should "be converted to some list of numbers" in {
    val inputTerm = pair(two, pair(one, pair(zero, zero)))
    val expectedList = Some(List(2, 1, 0))

    decodeListOfNumbers(inputTerm) should be (expectedList)
  }

  "A church list of ASCII codes" should "be converted to a string" in {
    val ninetySeven = encodeNumber(97)
    val ninetyEight = encodeNumber(98)
    val ninetyNine = encodeNumber(99)
    val abc = pair(ninetySeven, pair(ninetyEight, pair(ninetyNine, zero)))

    decodeString(abc) should be (Some("abc"))
  }
}
