package me.rexim.morganey.reduction

import me.rexim.morganey.ast._
import me.rexim.morganey.church.ChurchPairConverter._
import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.reduction.CallByName._
import org.scalatest._

class CallByNameSpec extends FlatSpec with Matchers with TestTerms {
  private val funcWithRedexBody = lfunc("x", redex(x))

  private val variableWithoutRedex = x
  private val functionWithoutRedex = I(x)
  private val applicationWithoutRedex = lapp(x, x)

  "Term with a redex in a head position" should "be considered unfinished" in {
    assert(!redex(x).cbnIsFinished())
    assert(!lapp(redex(x), y).cbnIsFinished())
  }

  it should "be reduced by one step of the strategy" in {
    redex(x).cbnStepReduce() should be (x)
    lapp(redex(x), y).cbnStepReduce() should be (lapp(x, y))
  }

  "Term with the only redex inside of a lambda abstraction" should "be considered finished" in {
    assert(funcWithRedexBody.cbnIsFinished())
  }

  it should "be not reduced by one step of the strategy" in {
    funcWithRedexBody.cbnStepReduce() should be (funcWithRedexBody)
  }

  "Term without any redex" should "be considered finished" in {
    assert(variableWithoutRedex.cbnIsFinished())
    assert(functionWithoutRedex.cbnIsFinished())
    assert(applicationWithoutRedex.cbnIsFinished())
  }

  it should "be not reduced by one step of the strategy" in {
    variableWithoutRedex.cbnStepReduce() should be (variableWithoutRedex)
    functionWithoutRedex.cbnStepReduce() should be (functionWithoutRedex)
    applicationWithoutRedex.cbnStepReduce() should be (applicationWithoutRedex)
  }

  "Term with redices" should "be reduced until there are redices in head position" in {
    redex(redex(redex(funcWithRedexBody))).cbnReduce() should be (funcWithRedexBody)
  }

  "Input term" should "be evaluated once by the strategy" in {
    val input = "abc"
    decodePair(LambdaInput(() => input.toStream).cbnReduce()) should be (Some((encodeNumber('a'.toInt), LambdaInput(() => input.tail.toStream))))
  }
}
