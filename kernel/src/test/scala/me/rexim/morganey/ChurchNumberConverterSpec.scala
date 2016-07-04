package me.rexim.morganey

import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ChurchNumberConverterSpec extends FlatSpec with Matchers with TestTerms {
  "An identity function" should "be converted to None" in {
    decodeNumber(I(x)) should be (None)
  }

  "A Church number" should "be decoded to a regular number" in {
    decodeNumber(zero) should be (Some(0))
    decodeNumber(one) should be (Some(1))
    decodeNumber(two) should be (Some(2))
  }

  "A a regular number" should "be encoded to a Church number" in {
    encodeNumber(0) should be (zero)
    encodeNumber(1) should be (one)
    encodeNumber(2) should be (two)
  }
}
