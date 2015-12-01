package me.rexim

object BetaReduction {
  def reduction(r: LambdaTerm): LambdaTerm = {
    r match {
      case LambdaApp(LambdaFunc(x, t), s) => t.substitute(x -> s)
      case _ => r
    }
  }
}
