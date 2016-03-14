package me.rexim.morganey.strategy

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class CallByNameSpec extends FlatSpec with Matchers with TestTerms {
  import CallByName._

  private val funcWithRedexBody = lfunc("x", redex(x))

  private val variableWithoutRedex = x
  private val functionWithoutRedex = I(x)
  private val applicationWithoutRedex = lapp(x, x)

  "Term with a redex in a head position" should "be considered unfinished" in {
    assert(!isFinished(redex(x)))
    assert(!isFinished(lapp(redex(x), y)))
  }

  it should "be reduced by one step of the strategy" in {
    stepReduce(redex(x)) should be (x)
    stepReduce(lapp(redex(x), y)) should be (lapp(x, y))
  }

  "Term with the only redex inside of a lambda abstraction" should "be considered finished" in {
    assert(isFinished(funcWithRedexBody))
  }

  it should "be not reduced by one step of the strategy" in {
    stepReduce(funcWithRedexBody) should be (funcWithRedexBody)
  }

  "Term without any redex" should "be considered finished" in {
    assert(isFinished(variableWithoutRedex))
    assert(isFinished(functionWithoutRedex))
    assert(isFinished(applicationWithoutRedex))
  }

  it should "be not reduced by one step of the strategy" in {
    stepReduce(variableWithoutRedex) should be (variableWithoutRedex)
    stepReduce(functionWithoutRedex) should be (functionWithoutRedex)
    stepReduce(applicationWithoutRedex) should be (applicationWithoutRedex)
  }
}
