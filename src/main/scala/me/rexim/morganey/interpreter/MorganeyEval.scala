package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.LambdaTerm

case class MorganeyEval(context: InterpreterContext, result : Option[LambdaTerm] = None) {
  def flatMap(f: (InterpreterContext) => MorganeyEval): MorganeyEval = f(context)
}
