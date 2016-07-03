package me.rexim.morganey.helpers

import me.rexim.morganey.ast.{LambdaFunc, LambdaVar, LambdaTerm}
import me.rexim.morganey.ast.LambdaTermHelpers._

trait TestTerms {
  val x = LambdaVar("x")
  val y = LambdaVar("y")
  val z = LambdaVar("z")

  def alphaVar(prefix: String, number: Int) =
    LambdaVar(s"$prefix##$number")

  def I(v : LambdaVar) = LambdaFunc(v, v)

  def pair(first: LambdaTerm, second: LambdaTerm) =
    lfunc("z",
      lapp(
        lapp(lvar("z"), first),
        second))

  val random = scala.util.Random

  def redex(body: LambdaTerm) = {
    val id = s"##${random.nextInt(100500)}"
    lapp(lfunc(id, body), lvar(id))
  }
}
