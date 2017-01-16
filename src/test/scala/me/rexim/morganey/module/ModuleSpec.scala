package me.rexim.morganey.module

import org.scalatest._

class ModuleSpec extends FlatSpec with Matchers {
  "Module" should "return its correct name" in {
    new Module("a/b/c.mgn").name should be ("a.b.c")
  }
}
