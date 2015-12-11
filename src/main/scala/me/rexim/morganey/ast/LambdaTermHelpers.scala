package me.rexim.morganey.ast

object LambdaTermHelpers {
  def func(varNames: List[String], body: => LambdaTerm): LambdaTerm = {
    varNames.foldRight(body) {
      case (varName, acc) => LambdaFunc(LambdaVar(varName), acc)
    }
  }

  def func(varName: String, body: => LambdaTerm): LambdaTerm =
    func(List(varName), body)
}
