package me.rexim.morganey.funtests

import java.io.ByteArrayInputStream

import scala.language.postfixOps
import org.scalatest._

import scala.sys.process._

class ReplFuntest extends FlatSpec with Matchers {
  // TODO: REPL funtests support
  ignore should "be evaluated to 9" in {
    val input = new ByteArrayInputStream("2 3\n".getBytes("UTF-8"))
    val actualOutput = ("java -jar ./target/scala-2.11/morganey.jar" #< input).lineStream_!
    val expectedOutput = Seq(
      "λ> number: 9",
      "λ> "
    )

    actualOutput should be (expectedOutput)
  }
}
