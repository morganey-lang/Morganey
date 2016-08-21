package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.meta._
import org.scalatest._

class TermOutputHelperSpec extends FlatSpec with Matchers {
  val testData = List[(LambdaTerm, String)](
    m"(\\x . x)" -> "term: (λ x . x)",
    m"khooy" -> "term: khooy",
    m"(a b)" -> "term: (a b)",
    m"5" -> "number: 5",
    m"[1, 2, 3]" -> "numbers: [1,2,3]",
    m"'x'" -> "char: 'x'",
    m""""khooy"""" -> "string: \"khooy\""
  )

  "A term" should "be correctly identified and showed" in {
    for ((term, resultString) <- testData) {
      TermOutputHelper.smartShowTerm(term) should be (resultString)
    }
  }
}
