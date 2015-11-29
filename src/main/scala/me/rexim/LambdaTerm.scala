package me.rexim

sealed trait LambdaTerm

case class LambdaVariable(name: String) extends LambdaTerm

case class LambdaFunction(parameter: LambdaVariable, body: LambdaTerm) {
  def apply(argument: LambdaTerm): LambdaTerm = ???
}

case class LambdaApplication(f: LambdaFunction, argument: LambdaTerm)
