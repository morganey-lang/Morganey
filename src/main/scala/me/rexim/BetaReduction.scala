package me.rexim

object BetaReduction {
  def substitute(t: LambdaTerm, v: LambdaVar, r: LambdaTerm): LambdaTerm = {
    t match {
      case LambdaVar(name) if name == v.name => r
      case LambdaVar(name) if name != v.name => t
      case LambdaApp(leftTerm, rightTerm) =>
        LambdaApp(substitute(leftTerm, v, r), substitute(rightTerm, v, r))
      case LambdaFunc(LambdaVar(name), body) if name == v.name => t
      case LambdaFunc(LambdaVar(name), body) if name != v.name =>
        LambdaFunc(LambdaVar(name), substitute(body, v, r))
    }
  }

  def reduction(r: LambdaTerm): LambdaTerm = {
    r match {
      case LambdaApp(LambdaFunc(x, t), s) => substitute(t, x, s)
      case _ => r
    }
  }
}
