package me.rexim.morganey.reduction

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.reduction.NormalOrder._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class NormalOrderSpec extends FlatSpec with Matchers with TestTerms {
  "Term with redices at any positions" should "be considered unfinished" in {
    assert(!redex(x).norIsFinished())
    assert(!lapp(redex(x), y).norIsFinished())
    assert(!lfunc("x", redex(x)).norIsFinished())
    assert(!lapp(x, redex(y)).norIsFinished())
  }

  "Term without any redices at any positions" should "be considered finished" in {
    assert(x.norIsFinished())
    assert(I(x).norIsFinished())
    assert(lapp(x, y).norIsFinished())
  }

  "Leftmost outermost redex" should "be reduced by one step of the strategy" in {
    val t = lapp(x, redex(y))
    lfunc("x", redex(t)).norStepReduce() should be (lfunc("x", t))
  }

  "All redices" should "be reduced by the strategy" in {
    val inputTerm = redex(
      lfunc("x",
        redex(
          lapp(
            redex(redex(x)),
            redex(y)))))
    val expectedTerm = lfunc("x", lapp(x, y))

    inputTerm.norReduce() should be (expectedTerm)
  }
}
