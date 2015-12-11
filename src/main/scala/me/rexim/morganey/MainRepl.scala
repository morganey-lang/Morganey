package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.syntax.LambdaParser

object MainRepl {

  val globalContext = Map(
    // I := λx.x
    lvar("I") -> lfunc("x", lvar("x")),

    // K := λx.λy.x
    lvar("K") -> lnested(List("x", "y"), lvar("x")),

    // S := λx.λy.λz.x z (y z)
    lvar("S") -> lnested(List("x", "y", "z"),
      lapp(
        lapp(lvar("x"), lvar("z")),
        lapp(lvar("y"), lvar("z"))
      )
    ),

    // B := λx.λy.λz.x (y z)
    lvar("B") -> lnested(List("x", "y", "z"),
      lapp(lvar("x"), lapp(lvar("y"), lvar("z")))
    ),

    // C := λx.λy.λz.x z y
    lvar("C") -> lnested(List("x", "y", "z"),
      lapp(lapp(lvar("x"), lvar("z")), lvar("y"))
    )

    // W := λx.λy.x y y
    // U := λx.λy.y (x x y)
  )

  def main(args: Array[String]) = {
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()
      val termParseResult = LambdaParser.parse(LambdaParser.term, line)

      if (termParseResult.successful) {
        con.println(termParseResult.get.addContext(globalContext).normalOrder().toString)
      } else {
        con.println(termParseResult.toString)
      }
    }
  }
}
