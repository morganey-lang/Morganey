package me.rexim.morganey.interpreter

case class ReplResult[T](context: ReplContext, result : Option[T] = None)
