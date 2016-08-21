package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.church.ChurchPairConverter._

object TermOutputHelper {

  def isPrintableChar(code: Int): Boolean =
    32 <= code && code <= 176

  def smartShowTerm(term: LambdaTerm): String = {
    decodeNumber(term) match {
      case Some(number) if isPrintableChar(number) => s"char: '${number.toChar}'"
      case Some(number) => s"number: $number"
      case None => decodeListOfNumbers(term) match {
        case Some(numbers) if numbers.forall(isPrintableChar) =>
          s"""string: \"${numbers.map(_.toChar).mkString}\""""
        case Some(numbers) => s"numbers: [${numbers.mkString(",")}]"
        case None => s"term: $term"
      }
    }
  }
}
