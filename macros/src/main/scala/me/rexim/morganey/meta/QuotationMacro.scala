package me.rexim.morganey.meta

import me.rexim.morganey.ast._

import scala.reflect.macros.whitebox.Context
import StringContext.treatEscapes

private[meta] class QuotationMacro(val c: Context) {
  import c.universe._

  private type Lift[T]      = me.rexim.morganey.meta.Liftable[T]
  private type Unlift[T]    = me.rexim.morganey.meta.Unliftable[T]
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
      b ++= treatEscapes(part)
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

  private def argument(i: Int): Tree = method match {
    case TermName("apply")   => quoteArg(i)
    case TermName("unapply") => unquoteArg(i)
  }

  private def quoteArg(i: Int): Tree = {
    val arg = args(i)
    val tpe = arg.tpe
    if (tpe <:< typeOf[LambdaTerm]) {
      arg
    } else {
      val liftT = appliedType(typeOf[Lift[_]], tpe)
      val lift  = c.inferImplicitValue(liftT, silent = true)
      if (lift.nonEmpty) {
        q"$lift($arg)"
      } else {
        val reason = s"Because no implicit value of type me.rexim.morganey.meta.Liftable[$tpe] could be found!"
        val msg    = s"Couldn't lift a value of type $tpe to lambda term! ($reason)"
        c.abort(arg.pos, msg)
      }
    }
  }

  private def unquoteArg(i: Int): Tree = {
    val x = TermName(s"x$i")
    val subpattern = c.internal.subpatterns(args.head).get.apply(i)
    subpattern match {
      case pq"$_: $tpt" =>
        val tpe = c.typecheck(tpt, c.TYPEmode).tpe
        val UnliftT = appliedType(typeOf[Unlift[_]], tpe)
        val unlift  = c.inferImplicitValue(UnliftT, silent = true)
        if (unlift.nonEmpty) {
          pq"$unlift($x @ _)"
        } else {
          val reason = s"Because no implicit value of type me.rexim.morganey.meta.Unliftable[$tpe] could be found!"
          val msg    = s"Couldn't unlift a lambda term to a value of type $tpe! ($reason)"
          c.abort(subpattern.pos, msg)
        }
      case _ =>
        pq"$x @ _"
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
    val ps = parts.indices.init
    lazy val (ifp, elp) = parts match {
      case Nil      =>
        c.abort(c.enclosingPosition, "Internal error: \"parts\" is empty.")
      case hd :: Nil =>
        (q"true", q"false")
      case _       =>
        val ys = ps map { i =>
          TermName(s"x$i")
        }
        (q"_root_.scala.Some((..$ys))", q"_root_.scala.None")
    }

    val identName = TermName(c.freshName())
    q"""
      new {
        val $identName = true
        def unapply(input: $LambdaTermTpe) = input match {
          case $term if $identName => $ifp
          case _                   => $elp
        }
      }.unapply(..$args)
    """
  }

  private def expand() = transform(parse(buildProgram()))

  def quote(args: Tree*): Tree = expand()
  def unquote(arg: Tree): Tree = expand()

}
