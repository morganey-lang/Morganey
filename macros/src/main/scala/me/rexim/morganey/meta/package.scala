package me.rexim.morganey

import scala.language.experimental.macros

package object meta extends AnyRef
  with DefaultLiftableInstances {

  private[meta] trait Quasiquotation {
    def apply(args: Any*): Any = macro QuotationMacro.quote
    def unapply(): Any = ???
  }

  implicit class Quasiquote(ctx: StringContext) {
    object m extends Quasiquotation
    object lc extends Quasiquotation
  }

}
