package me.rexim.morganey.funtests

import java.io.ByteArrayInputStream

import org.scalatest._

class SamplesFuntest extends FlatSpec with Matchers with MorganeyProcess {
  "Hello World sample" should "print Hello World message" in {
    val expectedOutput = s"""string: \"Hello, World\"${System.lineSeparator()}"""
    val actualOutput = morganey("./docs/samples/01-hello-world.mgn").!!
    actualOutput should be (expectedOutput)
  }

  "Multiplication sample" should "print the result of 2 * 3" in {
    val expectedOutput = s"number: 6${System.lineSeparator()}"
    val actualOutput = morganey("./docs/samples/02-multiplication.mgn").!!
    actualOutput should be (expectedOutput)
  }

  "Echo sample" should "print back the user's input" in {
    val input = new ByteArrayInputStream("Hello".getBytes("UTF-8"))
    val expectedOutput = s"""string: \"Hello\"${System.lineSeparator()}"""
    val actualOutput = morganey("./docs/samples/03-echo.mgn") #< input !!

    actualOutput should be (expectedOutput)
  }
}
