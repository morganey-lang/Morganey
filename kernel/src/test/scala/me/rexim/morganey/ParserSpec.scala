package me.rexim.morganey

import me.rexim.morganey.ast.LambdaApp
import me.rexim.morganey.church.ChurchNumberConverter._
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
    "load foo.bar",
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


  "Morganey module" should "consist of loadings and bindings only" in {
    val validModule =
      """
        |load foo
        |load bar
        |x := 1
      """.stripMargin

    val invalidModule =
      """
        |load foo
        |load bar
        |(x (y z))
        |x := 1
      """.stripMargin

    LambdaParser.parseAll(LambdaParser.module, validModule).successful should be (true)
    LambdaParser.parseAll(LambdaParser.module, invalidModule).successful should be (false)
  }

  "Empty list literals" should "desugar into zero" in {
    val res = LambdaParser.parseAll(LambdaParser.term, "[]")
    res.successful should be (true)
    res.get        should be (zero)
  }

  "Singleton list literals" should "desugar into a single pair" in {
    val res1 = LambdaParser.parseAll(LambdaParser.term, "[0]")
    res1.successful should be (true)
    res1.get        should be (pair(zero, zero, "x"))

    val res2 = LambdaParser.parseAll(LambdaParser.term, "['a']")
    res2.successful should be (true)
    res2.get        should be (pair(encodeNumber('a'), zero, "x"))
  }

  "Non empty list literals" should "desugar into nested pairs" in {
    val res1 = LambdaParser.parseAll(LambdaParser.term, "[0, 1, 2]")
    res1.successful should be (true)
    res1.get        should be (pair(zero, pair(one, pair(two, zero, "x"), "x"), "x"))

    val res2 = LambdaParser.parseAll(LambdaParser.term, "['a', 'b', 'c']")
    res2.successful should be (true)
    res2.get        should be (pair(encodeNumber('a'), pair(encodeNumber('b'), pair(encodeNumber('c'), zero, "x"), "x"), "x"))
  }

  "List literals constructed by ascending ranges, whose bounds are literals" should "desugar into nested pairs" in {
    val res1 = LambdaParser.parseAll(LambdaParser.term, "[0 .. 2]")
    res1.successful should be (true)
    res1.get        should be (pair(zero, pair(one, pair(two, zero, "x"), "x"), "x"))

    val res2 = LambdaParser.parseAll(LambdaParser.term, "[0, 1 .. 2]")
    res2.successful should be (true)
    res2.get        should be (pair(zero, pair(one, pair(two, zero, "x"), "x"), "x"))

    val res3 = LambdaParser.parseAll(LambdaParser.term, "[0, 2 .. 2]")
    res3.successful should be (true)
    res3.get        should be (pair(zero, pair(two, zero, "x"), "x"))
  }

  "List literals constructed by descending ranges, whose bounds are literals" should "desugar into nested pairs" in {
    val res1 = LambdaParser.parseAll(LambdaParser.term, "[2 .. 0]")
    res1.successful should be (true)
    // Empty, because 'step' was not given
    res1.get        should be (zero)

    val res2 = LambdaParser.parseAll(LambdaParser.term, "[2, 1 .. 0]")
    res2.successful should be (true)
    res2.get        should be (pair(two, pair(one, pair(zero, zero, "x"), "x"), "x"))

    val res3 = LambdaParser.parseAll(LambdaParser.term, "[2, 0 .. 0]")
    res3.successful should be (true)
    res3.get        should be (pair(two, pair(zero, zero, "x"), "x"))
  }

  "Nested list literals" should "desugar also into nested pairs" in {
    val res = LambdaParser.parseAll(LambdaParser.term, "[[1, 2], 'a', [1, []]]")
    val fst = pair(one, pair(two, zero, "x"), "x")
    val snd = encodeNumber('a')
    val nil = zero
    val trd = pair(one, pair(nil, zero, "x"), "x")
    res.successful should be (true)
    res.get        should be (pair(fst, pair(snd, pair(trd, zero, "x"), "x"), "x"))
  }

  "List literals constructed by ranges, whose bounds are literals" can "be defined with number-like values" in {
    val a = encodeNumber('a')
    val b = encodeNumber('b')
    val c = encodeNumber('c')

    val abc = pair(a, pair(b, pair(c, zero, "x"), "x"), "x")

    val res1 = LambdaParser.parseAll(LambdaParser.term, "['a' .. 99]")
    res1.successful should be (true)
    res1.get        should be (abc)

    val res2 = LambdaParser.parseAll(LambdaParser.term, "[97 .. 'c']")
    res2.successful should be (true)
    res2.get        should be (abc)

    val res3 = LambdaParser.parseAll(LambdaParser.term, "[97, 98 .. 'c']")
    res3.successful should be (true)
    res3.get        should be (abc)

    val res4 = LambdaParser.parseAll(LambdaParser.term, "[97, 'b' .. 'c']")
    res4.successful should be (true)
    res4.get        should be (abc)
  }

}
