package me.rexim.morganey.strategy

import me.rexim.morganey.ast.LambdaTerm

trait Strategy {
  def stepReduce(term: LambdaTerm): LambdaTerm
  def isFinished(term: LambdaTerm): Boolean
}
