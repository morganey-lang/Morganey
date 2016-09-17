package me.rexim.morganey.interpreter

case class ReplResult[+T](context: ReplContext, result : Option[T] = None) {

  def map[U](f: T => U): ReplResult[U] =
    ReplResult(context, result map f)

}
