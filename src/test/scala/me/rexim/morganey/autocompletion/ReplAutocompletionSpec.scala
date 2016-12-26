package me.rexim.morganey.autocompletion

import org.scalatest._

class ReplAutocompletionSpec extends FlatSpec with Matchers {
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

  "matches" should "answer true on empty name and any definition" in {
    ReplAutocompletion.matches("", "") should be (true)
    ReplAutocompletion.matches("khooy", "") should be (true)
  }

  it should "answer true if name is a prefix of definition" in {
    ReplAutocompletion.matches("foo", "foo") should be (true)
    ReplAutocompletion.matches("foobar", "foo") should be (true)
  }

  it should "answer false if name is not a prefix of definition" in {
    ReplAutocompletion.matches("", "foo") should be (false)
    ReplAutocompletion.matches("bar", "foo") should be (false)
    ReplAutocompletion.matches("barfoo", "foo") should be (false)
  }

  it should "answer true if name is a prefix of definition case-insensitively" in {
    ReplAutocompletion.matches("FOO", "foo") should be (true)
    ReplAutocompletion.matches("fOobar", "foo") should be (true)
  }
}
