package me.rexim.morganey.autocompletion

import org.scalatest._

class LastNameInLineSpec extends FlatSpec with Matchers {
  "lastNameInLine" should "extract identifier suffix if it is present" in {
    ReplAutocompletion.lastNameInLine("hello") should be (Some((0, "hello")))
    ReplAutocompletion.lastNameInLine("#$%^ hello") should be (Some(5, "hello"))
    ReplAutocompletion.lastNameInLine("$#*(&* hello5") should be (Some(7, "hello5"))
  }

  it should "return nothing if there is no identifier suffix" in {
    ReplAutocompletion.lastNameInLine("") should be (None)
    ReplAutocompletion.lastNameInLine("hello #$%^") should be (None)
    ReplAutocompletion.lastNameInLine("#$%^ hello &#*$#") should be (None)
  }
}
