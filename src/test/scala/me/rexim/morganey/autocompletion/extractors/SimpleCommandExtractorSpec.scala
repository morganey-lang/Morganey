package me.rexim.morganey.autocompletion.extractors

import org.scalatest._

class SimpleCommandExtractorSpec extends FlatSpec with Matchers {
  "SimpleCommand extractor" should "not extract not commands" in {
    SimpleCommand.unapply("khooy") should be (None)
  }

  it should "extract command without argument" in {
    SimpleCommand.unapply(":khooy") should be (Some("khooy"))
  }

  it should "extract command with argument" in {
    SimpleCommand.unapply(":khooy foo") should be (Some("khooy"))
  }
}
