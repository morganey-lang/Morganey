import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.church.numbers.ChurchPairConverter._
import me.rexim.morganey.syntax.LambdaParser

def parseTerm(s: String): Option[LambdaTerm] = {
  val parseResult =
    LambdaParser.parse(LambdaParser.term, s)

  if (parseResult.successful)
    Some(parseResult.get)
  else
    None
}

// (λ z . ((z x) y))
def pair(first: LambdaTerm, second: LambdaTerm) =
  lfunc("z",
    lapp(
      lapp(lvar("z"), first),
      second))

def succ(number: LambdaTerm) = {
  // SUCC = λn.λf.λx.f (n f x)
  val internalSucc = parseTerm("(λn.(λf.(λx.(f ((n f) x)))))")
  internalSucc.map(lapp(_, number).normalOrder())
}


def plus(x: LambdaTerm, y: LambdaTerm) = {
  // PLUS := λm.λn.λf.λx.m f (n f x)
  val internalPlus =
    parseTerm("(λm.(λn.(λf.(λx.((m f) ((n f) x))))))")

  internalPlus.map(f => lapp(lapp(f, x), y).normalOrder())
}

// MULT := λm.λn.λf.m (n f)
def mult(x: LambdaTerm, y: LambdaTerm) = {
  val internalMult = parseTerm("(λm.(λn.(λf.(m (n f)))))")
  internalMult.map(f => lapp(lapp(f, x), y).normalOrder())
}

val zero = parseTerm("(\\f . (\\x . x))").get
val one = succ(zero).get
val two = succ(one).get
val three = succ(two).get
val seven = plus(three, three).flatMap(succ).get
val nine = mult(three, three).get
val ten = succ(nine).get

val ninetySeven =
  mult(nine, ten)
    .flatMap(plus(_, seven))
    .get
val ninetyEight =
  succ(ninetySeven)
    .get
val ninetyNine =
  succ(ninetyEight)
    .get

val abc = pair(ninetySeven, pair(ninetyEight, ninetyNine))

convertString(abc)
