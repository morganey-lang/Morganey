package me.rexim.morganey.church

import me.rexim.morganey.ast.{LambdaApp, LambdaFunc, LambdaVar, LambdaTerm}
import me.rexim.morganey.ast.LambdaTerm

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

  def decodeList(list: LambdaTerm): List[LambdaTerm] =
    decodePair(list) match {
      case Some((first, second)) => first :: decodeList(second)
      case None                  => List(list)
    }

  /** Returns `None`, if list is empty */
  def encodeList(xs: List[LambdaTerm]): Option[LambdaTerm] = xs match {
    case init :+ last => Some(init.foldRight(last) { case (a, term) => encodePair((a, term)) })
    case _            => None
  }

  def decodeListOfNumbers(list: LambdaTerm): Option[List[Int]] = {
    decodeList(list)
      .map(ChurchNumberConverter.decodeNumber)
      .foldRight[Option[List[Int]]] (Some(List[Int]())) {
        case (x, acc) => acc.flatMap(xs => x.map(s => s :: xs))
      }
  }

  def decodeString(s: LambdaTerm): Option[String] =
    decodeListOfNumbers(s).map {
      xs => xs.map(_.toChar).mkString
    }
}
