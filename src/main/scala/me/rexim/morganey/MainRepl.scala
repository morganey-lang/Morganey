package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast.{LambdaApp, LambdaFunc, LambdaVar}
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.syntax.LambdaParser

object MainRepl {

  val globalContext = Map(
    // I := λx.x
    LambdaVar("I") -> LambdaFunc(LambdaVar("x"),
      LambdaVar("x")
    ),

    // K := λx.λy.x
    LambdaVar("K") -> func(List("x", "y"), LambdaVar("x")),

    // S := λx.λy.λz.x z (y z)
    LambdaVar("S") -> func(List("x", "y", "z"),
      LambdaApp(
        LambdaApp(LambdaVar("x"), LambdaVar("z")),
        LambdaApp(LambdaVar("y"), LambdaVar("z"))
      )
    ),

    // B := λx.λy.λz.x (y z)
    LambdaVar("B") -> LambdaFunc(LambdaVar("x"),
      LambdaFunc(LambdaVar("y"),
        LambdaFunc(LambdaVar("z"),
          LambdaApp(LambdaVar("x"),
            LambdaApp(LambdaVar("y"), LambdaVar("z")))
        )
      )
    )

    // C := λx.λy.λz.x z y
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
