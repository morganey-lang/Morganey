package me.rexim.morganey

import scala.language.experimental.macros

package object meta {

  def \(args: Any*): Any = macro TestMacro.quote

  def λ(args: Any*): Any = macro TestMacro.quote

  def m(args: Any*): Any = macro TestMacro.quote

}
