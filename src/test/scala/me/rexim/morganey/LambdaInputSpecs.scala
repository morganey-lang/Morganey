package me.rexim.morganey

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.ChurchPairConverter._
import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class LambdaInputSpecs extends FlatSpec with Matchers with TestTerms {
  val longInput = LambdaInput("khooy".toStream)
  val singleCharInput = LambdaInput("k".toStream)

  "Lambda input" should "have zero free vars by definition" in {
    longInput.freeVars.isEmpty should be (true)
  }

  "Lambda input" should "be lazily evaluated to a pair when substituted" in {
    val forcedInput = decodePair(longInput.substitute(lvar("x") -> lvar("x")))
    forcedInput.isDefined should be (true)

    val firstChar = decodeChar(forcedInput.get._1)
    firstChar should be (Some('k'))
  }

  "Lambda input with single character" should "be evaluated to a single character" in {
    val forcedInput = decodeChar(singleCharInput.substitute(lvar("x") -> lvar("x")))
    forcedInput should be (Some('k'))
  }

  "Empty lambda input" should "stay unimplemented because of Morganey lists nature" in {
    intercept[NotImplementedError] {
      LambdaInput(Stream.empty).substitute(lvar("x") -> lvar("x"))
    }
  }
}
