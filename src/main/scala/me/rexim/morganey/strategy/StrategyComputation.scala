package me.rexim.morganey.strategy

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.computation.{ComputationCancelledException, Computation}

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

class StrategyComputation(base: LambdaTerm, val strategy: Strategy) extends Computation[LambdaTerm] {
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
