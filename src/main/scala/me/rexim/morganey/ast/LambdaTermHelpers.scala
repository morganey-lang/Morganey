package me.rexim.morganey.ast

object LambdaTermHelpers {
  def lfunc(varNames: List[String], body: => LambdaTerm): LambdaTerm = {
    varNames.foldRight(body) {
      case (varName, acc) => LambdaFunc(LambdaVar(varName), acc)
    }
  }

  def lfunc(varName: String, body: => LambdaTerm): LambdaTerm =
    lfunc(List(varName), body)

  def lvar(varName: String): LambdaTerm = LambdaVar(varName)
}
