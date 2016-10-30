package me.rexim.morganey.funtests

import org.scalatest._

class SamplesFuntest extends FlatSpec with Matchers with MorganeyProcess {
  "Hello World sample" should "print Hello World message" in {
    val expectedOutput = s"""string: \"Hello, World\"${System.lineSeparator()}"""
    val actualOutput = morganey("./docs/samples/01-hello-world.mgn").!!
    actualOutput should be (expectedOutput)
  }
}
