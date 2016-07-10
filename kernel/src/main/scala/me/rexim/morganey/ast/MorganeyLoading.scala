package me.rexim.morganey.ast

case class MorganeyLoading(modulePath: String) extends MorganeyNode {
  override def toString = s"load $modulePath"
}
