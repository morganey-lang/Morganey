package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.LambdaTerm

case class ReplResult(context: ReplContext, result : Option[LambdaTerm] = None) {
  def flatMap(f: (ReplContext) => ReplResult): ReplResult = f(context)
}
