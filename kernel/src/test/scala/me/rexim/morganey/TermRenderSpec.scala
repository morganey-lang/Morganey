package me.rexim.morganey

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.util._
import org.scalatest._

class TermRenderSpec extends FlatSpec with Matchers with TestTerms {
  val ab   = lapp(lvar("a"), lvar("b"))
  val abc  = lapp(ab, lvar("c"))
  val abcd = lapp(abc, lvar("d"))

  "A variable" should "be rendered to its name" in {
    x.toString should be ("x")
  }


  "An simple application" should "be rendered to lambda notation" in {
    LambdaApp(x, y).toString should be ("x y")
  }

  "Simple nested applications" should "be rendered to lambda notation" in {
    ab.toString   should be ("a b")
    abc.toString  should be ("a b c")
    abcd.toString should be ("a b c d")
  }

  "An abstraction in an application" should "be rendered in parenthesis" in {
    val abs = lfunc("a", x)
    lapp(abs, z).toString   should be ("(λa.x) z")
    lapp(abs, abs).toString should be ("(λa.x) (λa.x)")
    lapp(z, abs).toString   should be ("z (λa.x)")
  }

  "Applications, which do exist in nested applications" should "be rendered in parenthesis, if they are the deepest left-hand-side" in {
    lapp(x, ab).toString   should be ("x (a b)")
    lapp(x, abc).toString  should be ("x (a b c)")
    lapp(x, abcd).toString should be ("x (a b c d)")

    lapp(lapp(x, ab), y).toString   should be ("x (a b) y")
    lapp(lapp(x, abc), y).toString  should be ("x (a b c) y")
    lapp(lapp(x, abcd), y).toString should be ("x (a b c d) y")

    lapp(lapp(lapp(x, ab), ab), y).toString  should be ("x (a b) (a b) y")
    lapp(lapp(lapp(x, abc), ab), y).toString should be ("x (a b c) (a b) y")
    lapp(lapp(lapp(x, ab), abc), y).toString should be ("x (a b) (a b c) y")
  }


  "A simple abstraction" should "be rendered to lambda notation" in {
    I(x).toString should be ("λx.x")
  }

  "Simple nested abstraction" should "be rendered to lambda notation" in {
    lnested(List("a", "b"),           lvar("x")).toString should be ("λa.b.x")
    lnested(List("a", "b", "c"),      lvar("x")).toString should be ("λa.b.c.x")
    lnested(List("a", "b", "c", "d"), lvar("x")).toString should be ("λa.b.c.d.x")
  }

  "An abstraction by itself" should "always be rendered without parenthesis" in {
    lfunc("a", x).toString                     should be ("λa.x")
    lnested(List("a", "b"), ab).toString       should be ("λa.b.a b")
    lnested(List("a", "b", "c"), abc).toString should be ("λa.b.c.a b c")
  }


  "A lazy sequence of characters" should "be rendered to \"<input>\"" in {
    LambdaInput(() => Stream.empty[Char]).toString should be ("<input>")
    LambdaInput(() => "foo bar".toStream).toString should be ("<input>")
  }

  "Parsing a term, and reparsing the result of the pretty-printer" should "always succeed" in {
    def parse(x: String) = LambdaParser.parseWith(x, _.term)

    val terms = Seq(
      "a", "xs", "foo",
      "a b", "a b c", "a b c d",
      "λx.y.z.x",
      "λx.y.z.x y",
      "λx.y.z.x y z",
      "(λa.b.c) z",
      "(λa.b) z",
      "(λa.b.c) (λa.b.c)",
      "a (b c) d",
      "a (b c d)",
      "a (b c d) e",
      "a (b c) (d e)",
      "λx.x (λf.x.x) (λf.x.x)",
      "λx.x (λf.x.x (λf.x.x))"
    )

    for (term <- terms) {
      val result1 = parse(term)
      result1.isSuccess should be (true)
      val pretty  = result1.get.toString
      val result2 = parse(pretty)
      result2.isSuccess should be (true)
      term              should be (pretty)
      result1.get       should be (result2.get)
    }
  }
}
