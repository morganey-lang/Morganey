package me.rexim.morganey.meta

import org.scalatest.{FlatSpec, Matchers}

class HoleSpec extends FlatSpec with Matchers {

  "A hole" should "mark positions, where lambda-terms were inserted during interpolation" in {
    Hole(0)   should be ("$hole0")
    Hole(1)   should be ("$hole1")
    Hole(2)   should be ("$hole2")
    Hole(999) should be ("$hole999")
  }

  it can "be used to get the number back, which was used to create the hole" in {
    val Hole(0)   = "$hole0"
    val Hole(1)   = "$hole1"
    val Hole(2)   = "$hole2"
    val Hole(999) = "$hole999"
  }

}
