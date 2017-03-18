package me.rexim.morganey

import org.scalatest._

class CommandsSpec extends FlatSpec with Matchers {
  "The validRegex function" should "give back a function to match strings, if a valid regex was given" in {
    val matcher1 = Commands.validRegex("[a-zA-Z]+")
    matcher1                      shouldBe a[Some[_]]
    matcher1 exists (_("fooBar")) should be (true)

    val matcher2 = Commands.validRegex("[*")
    matcher2                      should be (None)
  }
}
