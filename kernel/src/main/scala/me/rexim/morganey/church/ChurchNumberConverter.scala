package me.rexim.morganey.church

import me.rexim.morganey.ast.LambdaTermHelpers.lnested
import me.rexim.morganey.ast._
import hiddenargs._

import scala.annotation.tailrec

object ChurchNumberConverter {

  private val printableChars = Set(32 to 176 :_*).map(_.toChar)

  @hiddenargs
  @tailrec
  private def unwrapNumber(f: String, x: String, number: LambdaTerm, @hidden acc: Int = 0): Option[Int] =
    number match {
      case LambdaVar(x1) if x == x1 => Some(acc)
      case LambdaApp(LambdaVar(f1), restNumber)
        if f1 == f => unwrapNumber(f, x, restNumber, acc + 1)
      case _ => None
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

  def decodeChar(term: LambdaTerm): Option[Char] = {
    decodeNumber(term).map(_.toChar) filter (printableChars.contains _)
  }
}
