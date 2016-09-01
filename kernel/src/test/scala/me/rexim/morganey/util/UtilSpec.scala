package me.rexim.morganey.util

import me.rexim.morganey.util._
import org.scalatest._

class UtilSpec extends FlatSpec with Matchers {

  "The sequence function" should "swap the monadic context in" in {
    val numbers  = List(1 to 10: _*)
    val someNums = numbers.map(Option(_))

    sequence(List(None)) should be (None)
    sequence(None :: someNums) should be (None)
    sequence(List(None, None, None)) should be (None)

    sequence(List(Some(1))) should be (Some(List(1)))
    sequence(someNums) should be (Some(numbers))
  }

  "The validRegex function" should "give back a function to match strings, if a valid regex was given" in {
    val matcher1 = validRegex("[a-zA-Z]+")
    matcher1                      shouldBe a[Some[_]]
    matcher1 exists (_("fooBar")) should be (true)

    val matcher2 = validRegex("[*")
    matcher2                      should be (None)
  }

}
