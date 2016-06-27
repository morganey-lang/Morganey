package me.rexim.morganey.meta

import scala.reflect.macros.whitebox.Context

private[meta] class TestMacro(val c: Context) {
  import c.universe._

  def quote(args: Tree*): Tree =
    q"""Predef.println("Hello, Macros!")"""
}
