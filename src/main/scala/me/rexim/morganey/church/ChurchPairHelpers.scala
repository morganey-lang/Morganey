package me.rexim.morganey.church

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.ast.LambdaTermHelpers._

object ChurchPairHelpers {
  // (λ z . ((z x) y))
  def pair(first: LambdaTerm, second: LambdaTerm) =
    lfunc("z",
      lapp(
        lapp(lvar("z"), first),
        second))
}
