package me.rexim.morganey.ast

case class MorganeyLoading(modulePath: Option[String]) extends MorganeyNode {
  override def toString = modulePath.map(path => s"load $path").getOrElse("invalid load")
}
