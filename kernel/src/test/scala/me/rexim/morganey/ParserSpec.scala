package me.rexim.morganey

import me.rexim.morganey.ast.LambdaApp
import me.rexim.morganey.syntax._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers with TestTerms {
  "A lambda term" should "be parsed accordingly" in {
    val expression = "((λ x . x) (λ y . y))"
    val term = LambdaParser.parse(LambdaParser.term, expression)

    term.successful should be (true)
    term.get should be (LambdaApp(I(x), I(y)))
  }

  private val parse = LambdaParser.parseAll(LambdaParser.term, _: String)

  private val validPrograms = Seq(
    // string-literals
    """"Hello!"""",
    """"\tFoo\nBar!\n"""",
    "\"\\\"\"",
    "\"\\\'\"",

    // char-literals
    """'a'""",
    """'\n'""",
    """'''""",
    """'\''""",
    """'\"'""",
    """'"'""",

    // number-literals
    "0",
    "6",
    "42",

    "(λa.b)", "(a b)",
    "(λa.(a b))", "((a b) (a b))"
  )

  for (program <- validPrograms) {
    "Morganeys parser" should s"parse the valid program <$program> without any error" in {
      parse(program).successful should be (true)
    }
  }

  private val invalidPrograms = Seq(
    "\"Hello",
    "'a",
    "(a b",
    "(a . b)",
    "(\\\\a . b)"
  )

  for (program <- invalidPrograms) {
    "Morganeys parser" should s"parse the invalid program <$program> with a parse-error" in {
      parse(program).successful should be(false)
    }
  }

}
