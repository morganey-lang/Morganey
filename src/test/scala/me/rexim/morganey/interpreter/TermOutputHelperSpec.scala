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
    m""""khooy"""" -> "string: \"khooy\"",
    m"['a' .. 'f']" -> "string: \"abcdef\"",
    m"['a','c' .. 'm']" -> "string: \"acegikm\"",
    m"""["abc", "def"]""" -> "elements: [\"abc\",\"def\"]",
    m"""[["abc", "def"]]""" -> "elements: [[\"abc\",\"def\"]]",
    m"""[[1, 2, 3, 'a']]""" -> "elements: [[1,2,3,97]]",
    m"""['A', [1, 2], 1]""" -> "elements: ['A',[1,2],1]",
    m"""['A', ""]""" -> "numbers: [65,0]",
    m"[[1, 'A'], []]" -> "elements: [[1,65],0]"
  )

  "A term" should "be correctly identified and showed" in {
    for ((term, resultString) <- testData) {
      TermOutputHelper.smartShowTerm(term) should be (resultString)
    }
  }
}
