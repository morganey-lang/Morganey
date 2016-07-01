package me.rexim.morganey

import scala.language.experimental.macros

package object meta extends AnyRef
  with DefaultLiftableInstances
  with DefaultUnliftableInstances {

  private[meta] trait Quasiquotation {
    def apply[T](args: T*): Any = macro QuotationMacro.quote
    def unapply(arg: Any): Any = macro QuotationMacro.unquote
  }

  implicit class Quasiquote(ctx: StringContext) {
    object m extends Quasiquotation
    object lc extends Quasiquotation
  }

}
