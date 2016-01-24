package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTerm

import me.rexim.morganey.church.ChurchPairConverter._

object ReplHelper {
  def smartPrintTerm(term: LambdaTerm): String = {
    convertListOfNumbers(term) match {
      case Some(numbers) => if (numbers.forall(x => 32 <= x && x <= 176)) {
        s"string: ${numbers.map(_.toChar).mkString}"
      } else {
        s"numbers: ${numbers.mkString(",")}"
      }

      case None => s"term: ${term.toString}"
    }
  }
}
