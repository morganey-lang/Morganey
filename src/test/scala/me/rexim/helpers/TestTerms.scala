package me.rexim.helpers

import me.rexim.{LambdaFunc, LambdaVar}

trait TestTerms {
  val x = LambdaVar("x")
  val y = LambdaVar("y")
  val z = LambdaVar("z")

  def I(v : LambdaVar) = LambdaFunc(v, v)
}
