package me.rexim.morganey.church.numbers

import me.rexim.morganey.ast.{LambdaApp, LambdaVar, LambdaFunc, LambdaTerm}

object ChurchNumberConverter {

  private def unwrapNumber(f: String, x: String, number: LambdaTerm): Option[Int] = {
    number match {
      case LambdaVar(x1) if x == x1 => Some(0)
      case LambdaApp(LambdaVar(f1), restNumber)
        if f1 == f => unwrapNumber(f, x, restNumber).map(_ + 1)
      case _ => None
    }
  }

  def convert(term: LambdaTerm): Option[Int] = term match {
    case LambdaFunc(LambdaVar(f), LambdaFunc(LambdaVar(x), number)) =>
      unwrapNumber(f, x, number)
    case _ => None
  }
}
