package me.rexim.morganey.church

import me.rexim.morganey.ast.LambdaTermHelpers.{lnested, lvar, lfunc}
import me.rexim.morganey.ast._

import scala.annotation.tailrec

object ChurchNumberConverter {

  private def unwrapNumber(f: String, x: String, number: LambdaTerm): Option[Int] = {
    number match {
      case LambdaVar(x1) if x == x1 => Some(0)
      case LambdaApp(LambdaVar(f1), restNumber)
        if f1 == f => unwrapNumber(f, x, restNumber).map(_ + 1)
      case _ => None
    }
  }

  @tailrec
  private def wrapNumber(number: Int, acc: LambdaTerm = LambdaVar("x")): LambdaTerm = {
    if (number <= 0)
      acc
    else
      wrapNumber(number - 1, LambdaApp(LambdaVar("f"), acc))
  }

  def decodeNumber(term: LambdaTerm): Option[Int] = term match {
    case LambdaFunc(LambdaVar(f), LambdaFunc(LambdaVar(x), number)) =>
      unwrapNumber(f, x, number)
    case _ => None
  }

  def encodeNumber(number: Int): LambdaTerm = {
    lnested(List("f", "x"), wrapNumber(number))
  }
}
