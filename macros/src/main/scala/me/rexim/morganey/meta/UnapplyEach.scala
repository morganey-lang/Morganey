package me.rexim.morganey.meta

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.monad.sequence

class UnapplyEach[T](unliftT: Unliftable[T]) {

  def unapply(terms: List[LambdaTerm]): Option[List[T]] =
    sequence(terms map unliftT.unapply)

}
