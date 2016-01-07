package me.rexim.morganey.ast

case class MorganeyBinding(variable: LambdaVar, term: LambdaTerm) extends MorganeyNode
