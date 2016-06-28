package me.rexim.morganey

import scala.language.experimental.macros

package object meta {

  private[meta] trait Quasiquotation {
    def apply(args: Any*): Any = macro TestMacro.quote
    def unapply(): Any = ???
  }

  implicit class Quasiquote(ctx: StringContext) {
    object m extends Quasiquotation
    object lc extends Quasiquotation
  }

}
