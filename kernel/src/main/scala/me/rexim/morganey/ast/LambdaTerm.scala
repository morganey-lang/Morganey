package me.rexim.morganey.ast

import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.church.ChurchPairConverter._

sealed trait LambdaTerm extends MorganeyNode {
  val freeVars: Set[String]

  def substitute(substitution : (LambdaVar, LambdaTerm)): LambdaTerm

  def addBindings(context: Seq[MorganeyBinding]): LambdaTerm =
    context.filter(b => freeVars.contains(b.variable.name)).foldRight(this) {
      case (MorganeyBinding(variable, value), acc) =>
        LambdaApp(LambdaFunc(variable, acc), value)
    }
}

case class LambdaVar(name: String) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (name == v.name) r else this
  }

  override def toString = name

  override val freeVars: Set[String] = Set(name)
}

case class LambdaFunc(parameter: LambdaVar, body: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (parameter == v) {
      this
    } else if (r.freeVars.contains(parameter.name)) {
      val commonFreeVars = r.freeVars ++ body.freeVars

      val newParameter =
        LambdaVar(Stream.from(0)
          .map(number => s"${parameter.name}##$number")
          .dropWhile(commonFreeVars.contains)
          .head)

      val newBody = body.substitute(parameter -> newParameter)
      LambdaFunc(newParameter, newBody.substitute(v -> r))
    } else {
      LambdaFunc(parameter, body.substitute(v -> r))
    }
  }

  override def toString = s"(λ ${parameter.name} . $body)"

  override val freeVars: Set[String] = body.freeVars - parameter.name
}

case class LambdaApp(leftTerm: LambdaTerm, rightTerm: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    LambdaApp(
      leftTerm.substitute(v -> r),
      rightTerm.substitute(v -> r))
  }

  override def toString = s"($leftTerm $rightTerm)"

  override val freeVars: Set[String] = leftTerm.freeVars ++ rightTerm.freeVars
}

case class LambdaInput(input: Stream[Char]) extends LambdaTerm {
  override val freeVars: Set[String] = Set()

  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm =
    forceNextChar().substitute(substitution)

  /**
    * Forces next character in the input to be Church encoded and
    * put at the begining of the virtual input list
    */
  def forceNextChar(): LambdaTerm =
    input match {
      case x #:: xs => encodePair((encodeNumber(x.toInt), LambdaInput(xs)))
      case _ => encodeList(List())
    }

  override val toString = "<input>"
}
