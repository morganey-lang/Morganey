package me.rexim.morganey.meta

import scala.reflect.macros.whitebox.Context
import scala.util.{ Try => MTry }
import me.rexim.morganey.ast._

private[meta] class QuotationMacro(val c: Context) {
  import c.universe._

  private def macroApplication() = {
    val mCall = MTry(c.macroApplication).map {
      case q"$_($_(..${parts: List[String]})).m.apply(..$args)" => (parts, args)
    }
    val lCall = MTry(c.macroApplication).map {
      case q"$_($_(..${parts: List[String]})).lc.apply(..$args)" => (parts, args)
    }
    (mCall orElse lCall).get
  }

  private val (parts, args) = macroApplication()

  private def buildProgram(): String = {
    val pi = parts.iterator
    val ai = args.iterator
    val bldr = new StringBuilder(pi.next())
    var i = 0
    while (ai.hasNext) {
      val part = Hole(i)
      ai.next // skip element
      i += 1
      bldr ++= part
      bldr ++= pi.next()
    }
    bldr.toString
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

  private implicit def liftAst[A <: LambdaTerm]: Liftable[A] = Liftable {
    case LambdaVar(Hole(n))      =>
      args(n)
    case LambdaVar(name)         =>
      q"_root_.me.rexim.morganey.ast.LambdaVar($name)"
    case LambdaFunc(param, body) =>
      q"_root_.me.rexim.morganey.ast.LambdaFunc($param, $body)"
    case LambdaApp(left, right)  =>
      q"_root_.me.rexim.morganey.ast.LambdaApp($left, $right)"
  }

  def quote(args: Tree*): Tree =
    liftAst(parse(buildProgram()))

}

