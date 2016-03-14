package me.rexim.morganey.reduction

import me.rexim.morganey.ast.LambdaTerm

import scala.concurrent.Future

class StrategyComputation(base: LambdaTerm, strategy: Strategy) extends Computation[LambdaTerm] {
  @volatile var cancelled = false

  override def future: Future[LambdaTerm] = Future {
    var result: LambdaTerm = base

    while (!cancelled && !strategy.isFinished(result)) {
      result = strategy.stepReduce(result)
    }

    if (cancelled) {
      throw new ComputationCancelledException
    }

    result
  }

  override def cancel(): Unit = cancelled = true
}
