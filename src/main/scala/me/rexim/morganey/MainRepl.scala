package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.ChurchPairConverter
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
    ),

    // W := λx.λy.x y y
    lvar("W") -> lnested(List("x", "y"),
      lapp(lapp(lvar("x"), lvar("y")), lvar("y"))
    ),

    // U := λx.λy.y (x x y)
    lvar("U") -> lnested(List("x", "y"),
      lapp(
        lvar("y"),
        lapp(
          lapp(lvar("x"), lvar("x")),
          lvar("y")
        )
      )
    ),

    // ω := λx.x x
    lvar("omega") -> lfunc("x", lapp(lvar("x"), lvar("x"))),

    // Ω := ω ω
    lvar("Omega") -> lapp(lvar("omega"), lvar("omega")),

    // Y := λg.(λx.g (x x)) (λx.g (x x))
    lvar("Y") -> lfunc("g",
      lapp(
        lfunc("x", lapp(
          lvar("g"),
          lapp(lvar("x"), lvar("x"))
        )),
        lfunc("x", lapp(
          lvar("g"),
          lapp(lvar("x"), lvar("x"))
        ))
      )
    )
  )

  def main(args: Array[String]) = {
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()
      val termParseResult = LambdaParser.parse(LambdaParser.term, line)

      if (termParseResult.successful) {
        val result = termParseResult.get.addContext(globalContext).normalOrder()
        ChurchPairConverter.convertString(result) match {
          case Some(text) => con.println("string: " + text)
          case None => con.println("term: " + result.toString)
        }
      } else {
        con.println(termParseResult.toString)
      }
    }
  }
}
