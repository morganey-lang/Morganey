package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.ast._
import me.rexim.morganey.church.ChurchPairConverter
import me.rexim.morganey.syntax.LambdaParser

object MainRepl {

  var globalContext = List[MorganeyBinding]()

  def main(args: Array[String]) = {
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()
      val termParseResult = LambdaParser.parse(LambdaParser.replCommand, line)

      if (termParseResult.successful) {
        termParseResult.get match {
          case MorganeyBinding(variable, term) => {
            globalContext = MorganeyBinding(variable, term.addContext(globalContext).normalOrder()) :: globalContext
            con.println("Bound " + variable.name)
          }

          case term : LambdaTerm => {
            val result = term.addContext(globalContext).normalOrder()
            ChurchPairConverter.convertListOfNumbers(result) match {
              case Some(numbers) => if (numbers.forall(x => 32 <= x && x <= 176)) {
                con.println("string: " + numbers.map(_.toChar).mkString)
              } else {
                con.println("numbers: " + numbers.mkString(","))
              }
              case None => con.println("term: " + result.toString)
            }
          }
        }
      } else {
        con.println(termParseResult.toString)
      }
    }
  }
}
