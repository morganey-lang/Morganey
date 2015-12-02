package me.rexim

sealed trait LambdaTerm {
  def substitute(substitution : (LambdaVar, LambdaTerm)): LambdaTerm
  def reduce(): LambdaTerm
}

case class LambdaVar(name: String) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (name == v.name) r else this
  }

  override def reduce(): LambdaTerm = this
}

case class LambdaFunc(parameter: LambdaVar, body: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (parameter == v) this
    else LambdaFunc(parameter, body.substitute(v -> r))
  }

  override def reduce(): LambdaTerm = this
}

case class LambdaApp(leftTerm: LambdaTerm, rightTerm: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    LambdaApp(
      leftTerm.substitute(v -> r),
      rightTerm.substitute(v -> r))
  }

  override def reduce(): LambdaTerm = leftTerm.reduce() match {
    case LambdaFunc(x, t) => t.substitute(x -> rightTerm)
    case other => LambdaApp(other, rightTerm)
  }
}
