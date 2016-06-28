package me.rexim.morganey.meta

import scala.reflect.macros.whitebox.Context
import scala.util.{ Try => MTry, Success => MSucc }

private[meta] class TestMacro(val c: Context) {
  import c.universe._

  private def macroApplication() = {
    val mCall = MTry(c.macroApplication).map {
      case q"$_($_(..${parts: List[String]})).m.apply(..$args)" => (parts, args)
    }
    val lCall = MTry(c.macroApplication).map {
      case q"$_($_(..${parts: List[String]})).lc.apply(..$args)" => (parts, args)
    }
    (mCall orElse lCall).get
  }

  val (parts, args) = macroApplication()

  def quote(args: Tree*): Tree =
    q"""Predef.println("Hi, Macro")"""

}

