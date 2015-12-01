package me.rexim

sealed trait LambdaTerm

case class LambdaVar(name: String) extends LambdaTerm

case class LambdaFunc(parameter: LambdaVar, body: LambdaTerm) extends LambdaTerm {
  def apply(argument: LambdaTerm): LambdaTerm = ???
}

case class LambdaApp(leftTerm: LambdaTerm, rightTerm: LambdaTerm) extends LambdaTerm
