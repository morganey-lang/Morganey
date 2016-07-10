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

  private val parse = LambdaParser.parseAll(LambdaParser.script, _: String)

  "A program just containing comments" should "be a valid (but empty) program" in {
    val program =
      """
        | /*
        |  * This is an empty program
        |  */
        | // (\x.x)
      """.stripMargin

    val result = LambdaParser.parseAll(LambdaParser.script, program)
    result.successful should be (true)
    result.get        should be (Nil)
  }

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

    // comments
    "variable // this single-line comment is on the same line as a simple term",
    """
      |(\x . /*
      | * This multi-line comment is in the middle
      | * of a lambda-application.
      | */ x)
    """.stripMargin,

    // number-literals
    "0",
    "6",
    "42",

    "(λa.b)", "(a b)",
    "(λa.(a b))", "((a b) (a b))",

    // loads
    "load foo/bar",
    "load Foo10"
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
    "(\\\\a . b)",
    "load #&*$"
  )

  for (program <- invalidPrograms) {
    "Morganeys parser" should s"parse the invalid program <$program> with a parse-error" in {
      parse(program).successful should be(false)
    }
  }

}
