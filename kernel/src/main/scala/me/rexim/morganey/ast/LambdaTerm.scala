package me.rexim.morganey.ast

import me.rexim.morganey.church.ChurchNumberConverter._
import me.rexim.morganey.church.ChurchPairConverter._

import scala.annotation.tailrec

object LambdaTerm {

  private def nestedApplications(app: LambdaApp): (LambdaTerm, List[LambdaTerm]) = {
    @tailrec
    def go(app: LambdaApp, acc: List[LambdaTerm]): (LambdaTerm, List[LambdaTerm]) =
      app match {
        case LambdaApp(l: LambdaApp, r) => go(l, r :: acc)
        case LambdaApp(l, r)            => (l, r :: acc)
      }
    go(app, Nil)
  }

  private def nestedFunctions(func: LambdaFunc): (List[LambdaVar], LambdaTerm) = {
    @tailrec
    def go(func: LambdaFunc, acc: List[LambdaVar]): (List[LambdaVar], LambdaTerm) =
      func match {
        case LambdaFunc(v, f: LambdaFunc) => go(f, v :: acc)
        case LambdaFunc(v, b)             => ((v :: acc).reverse, b)
      }
    go(func, Nil)
  }

  private def hasAppAsBody(func: LambdaFunc): Boolean =
    nestedFunctions(func) match {
      case (_, _: LambdaApp) => true
      case _                 => false
    }

  def prettyPrint(term: LambdaTerm): String = term match {
    case LambdaVar(name)  => name
    case _: LambdaInput   => "<input>"
    case app: LambdaApp   =>
      val (deep, nest) = nestedApplications(app)
      val nested = nest.map {
        case a: LambdaApp                     => s"(${prettyPrint(a)})"
        case f: LambdaFunc if hasAppAsBody(f) => s"(${prettyPrint(f)})"
        case t                                => prettyPrint(t)
      }.mkString(" ")
      val rest = deep match {
        case f: LambdaFunc => s"(${prettyPrint(f)})"
        case t             => prettyPrint(t)
      }
      s"$rest $nested"
    case func: LambdaFunc =>
      val (vars, body) = nestedFunctions(func)
      val variables = vars.map(t => s"${prettyPrint(t)}.").mkString("")
      s"λ$variables${prettyPrint(body)}"
  }

}

sealed trait LambdaTerm extends MorganeyNode {
  val freeVars: Set[String]

  def substitute(substitution : (LambdaVar, LambdaTerm)): LambdaTerm

  def addBindings(context: Seq[MorganeyBinding]): LambdaTerm =
    context.foldRight(this) {
      case (MorganeyBinding(variable, value), acc) =>
        LambdaApp(LambdaFunc(variable, acc), value)
    }

  final override def toString: String =
    LambdaTerm.prettyPrint(this)
}

case class LambdaVar(name: String) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    if (name == v.name) r else this
  }

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

  override val freeVars: Set[String] = body.freeVars - parameter.name
}

case class LambdaApp(leftTerm: LambdaTerm, rightTerm: LambdaTerm) extends LambdaTerm {
  override def substitute(substitution: (LambdaVar, LambdaTerm)): LambdaTerm = {
    val (v, r) = substitution
    LambdaApp(
      leftTerm.substitute(v -> r),
      rightTerm.substitute(v -> r))
  }

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
}
