package me.rexim.morganey.ast

case class MorganeyBinding(variable: LambdaVar, term: LambdaTerm) extends MorganeyNode {
  override def toString = s"$variable := $term"
}
