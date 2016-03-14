package me.rexim.morganey.strategy

import me.rexim.morganey.ast.{LambdaFunc, LambdaApp, LambdaTerm}

object CallByName extends Strategy {
  override def stepReduce(term: LambdaTerm): LambdaTerm = term match {
    case LambdaApp(LambdaFunc(x, t), r) => t.substitute(x -> r)
    case LambdaApp(l, r) => LambdaApp(stepReduce(l), r)
    case other => other
  }

  override def isFinished(term: LambdaTerm): Boolean = term match {
    case LambdaApp(LambdaFunc(_, _), _) => false
    case LambdaApp(l, _) => isFinished(l)
    case _ => true
  }
}
