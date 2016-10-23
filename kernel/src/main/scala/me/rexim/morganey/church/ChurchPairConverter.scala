package me.rexim.morganey.church

import hiddenargs._
import me.rexim.morganey.ast.{LambdaApp, LambdaFunc, LambdaTerm, LambdaVar}
import me.rexim.morganey.church.ChurchNumberConverter.{decodeNumber, encodeNumber}
import me.rexim.morganey.monad._

object ChurchPairConverter {
  // (λ z . ((z x) y))
  def decodePair(pair: LambdaTerm): Option[(LambdaTerm, LambdaTerm)] =
    pair match {
      case LambdaFunc(LambdaVar(cons),
        LambdaApp(LambdaApp(LambdaVar(cons1), car), cdr)
      ) if cons == cons1 => Some((car, cdr))

      case _ => None
    }

  def encodePair(pair: (LambdaTerm, LambdaTerm)): LambdaTerm = {
    val (first, second) = pair
    val x = LambdaVar("x")

    val pairFunc = {
      val y = LambdaVar("y")
      val z = LambdaVar("z")

      LambdaFunc(y,
        LambdaFunc(z,
          LambdaApp(LambdaApp(z, x), y)))
    }

    pairFunc.substitute(x -> first) match {
      case LambdaFunc(y, body) => body.substitute(y -> second)
      case _ => throw new IllegalArgumentException("Capture free substitution produced unexpected result")
    }
  }

  @hiddenargs
  def decodeList(list: LambdaTerm, @hidden acc: List[LambdaTerm] = List()): Option[List[LambdaTerm]] =
    decodeNumber(list) match {
      case Some(0) => Some(acc.reverse)
      case _ => decodePair(list) match {
        case Some((first, second)) => decodeList(second, first :: acc)
        case _ => None
      }
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
