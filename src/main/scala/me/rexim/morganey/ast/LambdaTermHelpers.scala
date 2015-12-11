package me.rexim.morganey.ast

object LambdaTermHelpers {
  def lnested(varNames: List[String], body: LambdaTerm): LambdaTerm = {
    varNames.foldRight(body) {
      case (varName, acc) => lfunc(varName, acc)
    }
  }

  def lfunc(varName: String, body: LambdaTerm): LambdaFunc =
    LambdaFunc(lvar(varName), body)

  def lvar(varName: String): LambdaVar = LambdaVar(varName)

  def lapp(left: LambdaTerm, right: LambdaTerm): LambdaApp = LambdaApp(left, right)
}
