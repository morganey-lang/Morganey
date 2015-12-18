// A special place for experimenting with Morganey

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.numbers.ChurchNumberConverter.convertNumber
import me.rexim.morganey.church.numbers.ChurchPairConverter.{convertList, convertListOfNumbers, convertPair}

val succ = lnested(List("n", "f", "x"),
  lapp(
    lvar("f"),
    lapp(lapp(lvar("n"), lvar("f")), lvar("x"))))

val cons = lnested(List("first", "second"),
  lfunc("z",
    lapp(lapp(lvar("z"), lvar("first")), lvar("second"))))

def makeCons(car : LambdaTerm, cdr: LambdaTerm): LambdaTerm = {
  lfunc("z",
  lapp(lapp(lvar("z"), car), cdr))
}

val zero = lnested(List("f", "x"), lvar("x"))
val one = lapp(succ, zero).normalOrder()
val two = lapp(succ, one).normalOrder()

convertNumber(zero)
convertNumber(one)
convertNumber(two)
convertNumber(lapp(succ, one).normalOrder())

makeCons(two, makeCons(one, zero))

val listOfNumbers = lapp(lapp(cons, two), lapp(lapp(cons, one), zero)).normalOrder()

convertPair(listOfNumbers)
convertList(listOfNumbers)
convertListOfNumbers(listOfNumbers)