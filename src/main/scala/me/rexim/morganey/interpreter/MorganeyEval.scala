package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.LambdaTerm

case class MorganeyEval(context: ReplContext, result : Option[LambdaTerm] = None) {
  def flatMap(f: (ReplContext) => MorganeyEval): MorganeyEval = f(context)
}
