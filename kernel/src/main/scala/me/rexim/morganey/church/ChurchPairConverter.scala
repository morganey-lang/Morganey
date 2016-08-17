package me.rexim.morganey.church

import me.rexim.morganey.ast.{LambdaApp, LambdaFunc, LambdaVar, LambdaTerm}
import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.church.ChurchNumberConverter.{decodeNumber, encodeNumber}
import me.rexim.morganey.util._

object ChurchPairConverter {
  // (λ z . ((z x) y))
  def decodePair(pair: LambdaTerm): Option[(LambdaTerm, LambdaTerm)] =
    pair match {
      case LambdaFunc(LambdaVar(cons),
        LambdaApp(LambdaApp(LambdaVar(cons1), car), cdr)
      ) if cons == cons1 => Some((car, cdr))

      case _ => None
    }

  def encodePair(p: (LambdaTerm, LambdaTerm), cons: String = "x"): LambdaTerm = {
    val (car, cdr) = p
    LambdaFunc(LambdaVar(cons),
      LambdaApp(LambdaApp(LambdaVar(cons), car), cdr)
    )
  }

  def decodeList(list: LambdaTerm): Option[List[LambdaTerm]] = {
    def decodeListAgg(list: LambdaTerm, agg: List[LambdaTerm]): Option[List[LambdaTerm]] = {
      decodeNumber(list) match {
        case Some(0) => Some(agg)
        case _ => decodePair(list) match {
          case Some((first, second)) => decodeListAgg(second, first :: agg)
          case _ => None
        }
      }
    }

    decodeListAgg(list, List()).map(_.reverse)
  }

  def encodeList(xs: List[LambdaTerm]): LambdaTerm =
    xs.foldRight(encodeNumber(0)) { case (a, term) => encodePair((a, term)) }

  def decodeListOfNumbers(list: LambdaTerm): Option[List[Int]] = {
    decodeList(list).flatMap(xs => sequence(xs.map(decodeNumber)))
  }

  def encodeListOfNumbers(xs: List[Int]): LambdaTerm =
    encodeList(xs.map(encodeNumber))

  def decodeString(s: LambdaTerm): Option[String] =
    decodeListOfNumbers(s).map {
      xs => xs.map(_.toChar).mkString
    }

  def encodeString(s: String): LambdaTerm =
    encodeListOfNumbers(s.toList.map(_.toInt))
}
