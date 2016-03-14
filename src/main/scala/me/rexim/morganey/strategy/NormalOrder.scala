package me.rexim.morganey.strategy

import me.rexim.morganey.ast.{LambdaFunc, LambdaApp, LambdaTerm}

object NormalOrder extends Strategy {
  override def stepReduce(term: LambdaTerm): LambdaTerm = term match {
    case LambdaApp(LambdaFunc(x, t), r) => t.substitute(x -> r)
    case LambdaApp(l, r) if isFinished(l) => LambdaApp(stepReduce(l), r)
    case LambdaApp(l, r) if isFinished(r) => LambdaApp(l, stepReduce(r))
    case LambdaFunc(x, t) => LambdaFunc(x, stepReduce(t))
    case other => other
  }

  override def isFinished(term: LambdaTerm): Boolean = term match {
    case LambdaApp(LambdaFunc(_, _), _) => false
    case LambdaApp(l, r) => isFinished(l) && isFinished(r)
    case LambdaFunc(_, t) => isFinished(t)
    case _ => true
  }
}
