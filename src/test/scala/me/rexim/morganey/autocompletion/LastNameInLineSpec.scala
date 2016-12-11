package me.rexim.morganey.autocompletion

import org.scalatest._

class LastNameInLineSpec extends FlatSpec with Matchers {
  "lastNameInLine" should "extract identifier postfix if it is present" in {
    ReplAutocompletion.lastNameInLine("hello") should be (Some((0, "hello")))
    ReplAutocompletion.lastNameInLine("#$%^ hello") should be (Some(5, "hello"))
  }

  it should "return nothing if there is no identifier postfix" in {
    ReplAutocompletion.lastNameInLine("") should be (None)
    ReplAutocompletion.lastNameInLine("hello #$%^") should be (None)
    ReplAutocompletion.lastNameInLine("#$%^ hello &#*$#") should be (None)
  }
}
