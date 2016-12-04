package me.rexim.morganey.autocompletion.extractors

import org.scalatest._

class LoadStatementExtractorSpec extends FlatSpec with Matchers {
  "LoadStatement extractor" should "not extract empty string" in {
    LoadStatement.unapply("") should be (None)
  }

  "LoadStatement extractor" should "not extract load statements that start with dot" in {
    LoadStatement.unapply("load .") should be (None)
    LoadStatement.unapply("load .foo.bar") should be (None)
  }

  "LoadStatement extractor" should "extract load statement with empty path" in {
    LoadStatement.unapply("load ") should be (Some(List(), false))
  }

  "LoadStatement extractor" should "extract full and incomplete load statements" in {
    LoadStatement.unapply("load hello.world") should be (Some(List("hello", "world"), false))
    LoadStatement.unapply("load hello.world.") should be (Some(List("hello", "world"), true))
  }
}
