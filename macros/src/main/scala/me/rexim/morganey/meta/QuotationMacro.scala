package me.rexim.morganey.meta

import scala.reflect.macros.whitebox.Context
import me.rexim.morganey.ast._

private[meta] class QuotationMacro(val c: Context) {
  import c.universe._

  private type Lift[T]      = me.rexim.morganey.meta.Liftable[T]
  private val LambdaTermTpe = typeOf[LambdaTerm]

  private def macroApplication() =
    Option(c.macroApplication) collect {
      case q"$_($_(..${parts: List[String]})).m.$method[..$_](..$args)"  => (parts, method, args)
      case q"$_($_(..${parts: List[String]})).lc.$method[..$_](..$args)" => (parts, method, args)
    } getOrElse {
      c.abort(c.enclosingPosition, "Invalid usage of interpolator!")
    }

  private val (parts, method, args) = macroApplication()

  private def buildProgram(): String = {
    val b = new StringBuilder()
    val parts = this.parts.iterator
    var i = 0

    while (parts.hasNext) {
      val part = parts.next
      b ++= part
      if (parts.hasNext) {
        val hole = Hole(i)
        i += 1
        b ++= hole
      }
    }

    b.toString
  }

  private def parse(program: String): LambdaTerm = {
    val par = MetaParser
    par.parseAll(par.term, program) match {
      case par.Success(res, _)       => res
      case par.NoSuccess(err, input) =>
        val pos = input.pos; import pos._
        val msg = s"Error at line $line column $column in interpolated string:\n$err"
        c.abort(c.enclosingPosition, msg)
    }
  }

  private def argument(i: Int): Tree = {
    val arg = args(i)
    val tpe = arg.tpe
    if (tpe <:< typeOf[LambdaTerm]) {
      arg
    } else {
      val liftT = appliedType(typeOf[Lift[_]], tpe)
      val lift = c.inferImplicitValue(liftT, silent = true)
      if (lift.nonEmpty) {
        q"$lift($arg)"
      } else {
        val reason = s"Because no implicit value of type me.rexim.morganey.meta.Liftable[$tpe] could be found!"
        val msg    = s"Couldn't lift a value of type $tpe to lambda term! ($reason)"
        c.abort(arg.pos, msg)
      }
    }
  }

  private implicit def liftAst[A <: LambdaTerm]: Liftable[A] = Liftable {
    case LambdaVar(Hole(n))      =>
      argument(n)
    case LambdaVar(name)         =>
      q"_root_.me.rexim.morganey.ast.LambdaVar($name)"
    case LambdaFunc(param, body) =>
      q"_root_.me.rexim.morganey.ast.LambdaFunc($param, $body)"
    case LambdaApp(left, right)  =>
      q"_root_.me.rexim.morganey.ast.LambdaApp($left, $right)"
  }

  private def transform(term: LambdaTerm): Tree = method match {
    case TermName("apply")   => liftAst(term)
    case TermName("unapply") => extractor(term)
  }

  private def extractor(term: LambdaTerm): Tree = {
    q"""
      new {
        def unapply(input: $LambdaTermTpe) = input match {
          case $term => true
          case _     => false
        }
      }.unapply(..$args)
    """
  }

  private def expand() = transform(parse(buildProgram()))

  def quote(args: Tree*): Tree = expand()
  def unquote(arg: Tree): Tree = expand()

}

