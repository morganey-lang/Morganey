package me.rexim.morganey.reduction

import me.rexim.morganey.ast.LambdaTerm

trait Strategy {
  def stepReduce(term: LambdaTerm): LambdaTerm
  def isFinished(term: LambdaTerm): Boolean
}
