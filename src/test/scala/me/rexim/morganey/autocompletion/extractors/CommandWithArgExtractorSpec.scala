package me.rexim.morganey.autocompletion.extractors

import org.scalatest._
import me.rexim.morganey.Commands.{StringCommand, TermCommand}

class CommandWithArgExtractorSpec extends FlatSpec with Matchers {
  "CommandWithArg extractor" should "not extract non-commands" in {
    val extractor = new CommandWithArg(Map("khooy" -> StringCommand("khooy", null)))
    extractor.unapply("khooy") should be (None)
  }

  it should "not extract non-existing commands" in {
    val extractor = new CommandWithArg(Map())
    extractor.unapply(":khooy") should be (None)
  }

  it should "not extract existing string commands" in {
    val extractor = new CommandWithArg(Map("khooy" -> StringCommand("khooy", null)))
    extractor.unapply(":khooy") should be (None)
  }

  it should "extract existing term commands without arg" in {
    val extractor = new CommandWithArg(Map("khooy" -> TermCommand("khooy", null)))
    extractor.unapply(":khooy") should be (Some(("khooy", "")))
  }

  it should "extract existing term commands with arg" in {
    val extractor = new CommandWithArg(Map("khooy" -> TermCommand("khooy", null)))
    extractor.unapply(":khooy foo") should be (Some(("khooy", "foo")))
  }
}
