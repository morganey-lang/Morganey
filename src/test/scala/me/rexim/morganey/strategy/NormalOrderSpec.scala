package me.rexim.morganey.strategy

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class NormalOrderSpec extends FlatSpec with Matchers with TestTerms {
  import NormalOrder._

  "Term with redices at any positions" should "be considered unfinished" in {
    assert(!isFinished(redex(x)))
    assert(!isFinished(lapp(redex(x), y)))
    assert(!isFinished(lfunc("x", redex(x))))
    assert(!isFinished(lapp(x, redex(y))))
  }

  "Term without any redices at any positions" should "be considered finished" in {
    assert(isFinished(x))
    assert(isFinished(I(x)))
    assert(isFinished(lapp(x, y)))
  }

  "Leftmost outermost redex" should "be reduced by one step of the strategy" in {
    val t = lapp(x, redex(y))
    stepReduce(lfunc("x", redex(t))) should be (lfunc("x", t))
  }
}
