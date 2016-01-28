package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTerm

case class MorganeyEval(context: MorganeyInterpreter.Context = List(), result : Option[LambdaTerm] = None) {
  def flatMap(f: (MorganeyInterpreter.Context) => MorganeyEval): MorganeyEval = f(context)
}
