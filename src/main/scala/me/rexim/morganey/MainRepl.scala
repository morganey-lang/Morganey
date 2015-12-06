package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.syntax.LambdaParser

object MainRepl {
  def main(args: Array[String]) = {
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()
      val termParseResult = LambdaParser.parse(LambdaParser.term, line)

      if (termParseResult.successful) {
        con.println(termParseResult.get.reduce().toString)
      } else {
        con.println(termParseResult.toString)
      }
    }
  }
}
