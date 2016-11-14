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

  "ohcE sample" should "print the user's input backwards" in {
    val input = new ByteArrayInputStream("Hello".getBytes("UTF-8"))
    val expectedOutput = s"""string: \"olleH\"${System.lineSeparator()}"""
    val actualOutput = morganey("./docs/samples/04-ohce.mgn") #< input !!

    actualOutput should be (expectedOutput)
  }

  "15 first fibonacci numbers sample" should "print 15 first fibonacci numbers" in {
    val expectedOutput = s"numbers: [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]${System.lineSeparator()}"
    val actualOutput = morganey("./docs/samples/fib.mgn").!!
    actualOutput should be (expectedOutput)
  }
}
