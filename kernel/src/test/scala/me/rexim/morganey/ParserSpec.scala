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
}
