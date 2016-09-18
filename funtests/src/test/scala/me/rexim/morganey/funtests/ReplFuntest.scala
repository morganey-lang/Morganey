package me.rexim.morganey.funtests

import scala.language.postfixOps

import org.scalatest._

import scala.sys.process._

class ReplFuntest extends FlatSpec with Matchers {
  "2 3" should "be evaluated to 9" in {
    val actualOutput = "echo 2 3" #| "java -jar ./target/scala-2.11/morganey.jar" !!
    val expectedOutput = Seq(
      "λ> 2 3\n",
      "number: 9\n",
      "λ> \n"
    ).mkString("")

    actualOutput should be (expectedOutput)
  }
}
