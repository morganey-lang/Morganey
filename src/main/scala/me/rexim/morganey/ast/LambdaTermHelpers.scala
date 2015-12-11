package me.rexim.morganey.ast

object LambdaTermHelpers {
  def func(vars: List[String], body: => LambdaTerm): LambdaTerm = {
    vars.foldRight(body) {
      case (varName, acc) => LambdaFunc(LambdaVar(varName), acc)
    }
  }
}
