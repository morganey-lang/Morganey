package me.rexim.morganey

import me.rexim.morganey.ast.LambdaVar
import me.rexim.morganey.church.ChurchPairConverter
import me.rexim.morganey.church.ChurchPairConverter._
import me.rexim.morganey.helpers.TestTerms
import org.scalatest._

class ChurchPairEncodingSpec extends FlatSpec with Matchers with TestTerms {
  "Pair of vars `x` and `y`" should "be encoding into a Church pair" in {
    encodePair((x, y)) should be (pair(x, y, "z"))
  }

  "Pair of vars `z` and `y`" should "be capture free encoded into a Church pair" in {
    encodePair((z, y)) should be (pair(z, y, "z##0"))
  }

  "Pair of vars `z##0` and `z`" should "be capture free encoded into a Church pair" in {
    val z0 = LambdaVar("z##0")
    encodePair((z0, z)) should be (pair(z0, z, "z##1"))
  }
}
