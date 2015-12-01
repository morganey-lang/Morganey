package me.rexim

object BetaReduction {
  def substitute(term: LambdaTerm, substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    term match {
      case x: LambdaVar if x == v => r
      case x: LambdaVar if x != v => term
      case LambdaApp(leftTerm, rightTerm) =>
        LambdaApp(
          substitute(leftTerm, v -> r),
          substitute(rightTerm, v -> r))
      case LambdaFunc(x, body) if x == v => term
      case LambdaFunc(x, body) if x != v =>
        LambdaFunc(x, substitute(body, v -> r))
    }
  }

  def reduction(r: LambdaTerm): LambdaTerm = {
    r match {
      case LambdaApp(LambdaFunc(x, t), s) => substitute(t, x -> s)
      case _ => r
    }
  }
}
