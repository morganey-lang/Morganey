package me.rexim

object BetaReduction {
  def substitute(term: LambdaTerm, substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    term match {
      case LambdaVar(name) if name == v.name => r
      case LambdaVar(name) if name != v.name => term
      case LambdaApp(leftTerm, rightTerm) =>
        LambdaApp(substitute(leftTerm, v -> r), substitute(rightTerm, v -> r))
      case LambdaFunc(LambdaVar(name), body) if name == v.name => term
      case LambdaFunc(LambdaVar(name), body) if name != v.name =>
        LambdaFunc(LambdaVar(name), substitute(body, v -> r))
    }
  }

  def reduction(r: LambdaTerm): LambdaTerm = {
    r match {
      case LambdaApp(LambdaFunc(x, t), s) => substitute(t, x -> s)
      case _ => r
    }
  }
}
