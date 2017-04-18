package me.rexim.morganey.meta

import me.rexim.morganey.ast._

import StringContext.treatEscapes
import scala.reflect.macros.whitebox

private[meta] class QuotationMacro(val c: whitebox.Context) {
  import c.universe._

  def quote(args: Tree*): Tree = expand()
  def unquote(arg: Tree): Tree = expand()

  private def expand(): Tree = wrapAst(buildAndParseProgram())

  private def macroApplication() =
    Option(c.macroApplication) collect {
      case q"$_($_(..${parts: List[String]})).m.$method[..$_](..$args)"  => (parts, method, args)
      case q"$_($_(..${parts: List[String]})).lc.$method[..$_](..$args)" => (parts, method, args)
    } getOrElse {
      c.abort(c.enclosingPosition, "Invalid usage of interpolator!")
    }

  private val (parts, method, args) = macroApplication()

  private lazy val IterableClass: TypeSymbol =
    typeOf[Iterable[_]].typeSymbol.asType
  private lazy val IterableTParam: Type =
    IterableClass.typeParams.head.asType.toType
  private def iterableT(tpe: Type): Type =
    IterableTParam.asSeenFrom(tpe, IterableClass)

  private val LambdaTermTpe = typeOf[LambdaTerm]

  private lazy val liftList    = implicitly[Lift[List[LambdaTerm]]]
  private lazy val liftList_   = c.inferImplicitValue(typeOf[Lift[List[LambdaTerm]]], silent = true)
  private lazy val unliftList  = implicitly[Unlift[List[LambdaTerm]]]
  private lazy val unliftList_ = c.inferImplicitValue(typeOf[Unlift[List[LambdaTerm]]], silent = true)

  private type Lift[T] = me.rexim.morganey.meta.Liftable[T]
  private lazy val LiftableTpe = typeOf[Lift[_]]
  private def liftableT(tpe: Type): Type = appliedType(LiftableTpe, tpe)

  private type Unlift[T] = me.rexim.morganey.meta.Unliftable[T]
  private lazy val UnliftableTpe = typeOf[Unlift[_]]
  private def unliftableT(tpe: Type): Type = appliedType(UnliftableTpe, tpe)

  private case class Lifted(exp: Tree, preamble: List[Tree] = Nil) {
    def wrap(f: Tree => Tree): Lifted =
      Lifted(f(exp), preamble)

    def wrap2(other: Lifted)(f: (Tree, Tree) => Tree): Lifted =
      Lifted(f(exp, other.exp), preamble ++ other.preamble)
  }

  /**
   * Create the morganey source program, by joining the `parts` of the string context.
   * For each argument in `args` a hole is created. A hole is a special mangled identifier,
   * that won't be accepted by the default parser. The holes are later replaced by another
   * syntax tree.
   * The `MetaParser` allows parsing these special mangled identifiers.
   */
  private def buildAndParseProgram(): LambdaTerm = {
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

    import MetaParser._
    parseAll(term, b.toString) match {
      case Success(res, _)       => res
      case NoSuccess(err, input) =>
        val pos = input.pos; import pos._
        val msg = s"Error at line $line column $column in interpolated string:\n$err"
        c.abort(c.enclosingPosition, msg)
    }
  }

  private def liftPrimitiveTerm(term: LambdaTerm): Lifted = term match {
    case LambdaVar(DottedHole(_)) =>
      c.abort(c.enclosingPosition, "Illegal usage of ..!")
    case LambdaVar(Hole(hole)) =>
      replaceHole(hole, dotted = false)
    case LambdaVar(name) =>
      Lifted(q"_root_.me.rexim.morganey.ast.LambdaVar($name)")
    case LambdaFunc(param, body) =>
      liftPrimitiveTerm(param).wrap2(liftComplexTerm(body)) {
        case (paramTree, bodyTree) => q"_root_.me.rexim.morganey.ast.LambdaFunc($paramTree, $bodyTree)"
      }
    case LambdaApp(left, right) =>
      liftComplexTerm(left).wrap2(liftComplexTerm(right)) {
        case (leftTree, rightTree) => q"_root_.me.rexim.morganey.ast.LambdaApp($leftTree, $rightTree)"
      }
  }

  private def liftComplexTerm(term: LambdaTerm): Lifted = term match {
    case unliftList(terms) =>

      val unapply = TermName("unapply") == method

      def isDottedHole(term: LambdaTerm): Boolean = term match {
        case LambdaVar(DottedHole(_)) => true
        case _                        => false
      }

      def prepend(terms: List[LambdaTerm], tree: Lifted): Lifted =
        terms.foldRight(tree) {
          case (ele, acc) =>
            liftComplexTerm(ele).wrap2(acc) {
              case (a, b) if unapply => pq"$a :: $b"
              case (a, b)            => q"$a :: $b"
            }
        }

      def append(tree: Lifted, terms: List[LambdaTerm]): Lifted =
        terms.foldLeft(tree) {
          case (acc, ele) =>
            acc.wrap2(liftComplexTerm(ele)) {
              case (a, b) if unapply => pq"$a :+ $b"
              case (a, b)            => q"$a :+ $b"
            }
        }

      terms.span(!isDottedHole(_)) match {
        // [..$xs, _*]
        case (Nil, LambdaVar(DottedHole(hole)) :: rest) =>
          val holeContent = replaceHole(hole, dotted = true)
          val app = append(holeContent, rest)
          if (unapply) app.wrap(x => pq"$unliftList_($x)")
          else app.wrap(x => q"$liftList_($x)")

        // [_*, ..$xs]
        case (init, LambdaVar(DottedHole(hole)) :: Nil) =>
          val holeContent = replaceHole(hole, dotted = true)
          val prep = prepend(init, holeContent)
          if (unapply) prep.wrap(x => pq"$unliftList_($x)")
          else prep.wrap(x => q"$liftList_($x)")

        // no dotted list
        case (_, Nil) =>
          liftPrimitiveTerm(term)

        case _ =>
          c.abort(c.enclosingPosition, "Illegal usage of ..!")
      }

    case simple =>
      liftPrimitiveTerm(simple)
  }

  private def replaceHole(hole: Int, dotted: Boolean): Lifted = method match {
    case TermName("apply") =>
      val arg = args(hole)
      val tpe =
        if (!dotted) arg.tpe
        else iterableT(arg.tpe)

      def quote(tree: Tree): Tree =
        if (tpe <:< LambdaTermTpe) {
          /*
           * A value of type `LambdaTerm` will be inserted into the hole.
           */
          tree
        } else {
          /*
           * A value of type `tpe` will be inserted into the hole.
           * To be able to do so, it has to be converted to a value of type `LambdaTerm`.
           * Instances of the typeclass me.rexim.morganey.meta.Liftable know how to do the conversion.
           */
          val liftTpe = c.inferImplicitValue(liftableT(tpe), silent = true)
          if (liftTpe.nonEmpty) {
            q"$liftTpe($tree)"
          } else {
            val reason = s"Because no implicit value of type me.rexim.morganey.meta.Liftable[$tpe] could be found!"
            val msg    = s"Couldn't lift a value of type $tpe to lambda term! ($reason)"
            c.abort(arg.pos, msg)
          }
        }

      if (!dotted) {
        Lifted(quote(arg))
      } else {
        val parName = TermName(c.freshName())
        val list    = q"$arg.map { ($parName: $tpe) => ${quote(q"$parName")} }.toList"
        Lifted(list)
      }

    case TermName("unapply") =>
      val x = TermName(s"x$hole")
      val subpattern = c.internal.subpatterns(args.head).get.apply(hole)

      def unQuote(tpe: Type): Lifted = {
        val unliftTpe = c.inferImplicitValue(unliftableT(tpe), silent = true)

        if (unliftTpe.nonEmpty) {
          if (!dotted) {
            Lifted(pq"$unliftTpe($x @ _)")
          } else {
            val each = TermName(c.freshName(s"each$hole"))
            val preamble =
              q"""
                  val $each: _root_.me.rexim.morganey.meta.UnapplyEach[$tpe] =
                    new _root_.me.rexim.morganey.meta.UnapplyEach[$tpe]($unliftTpe)
                """
            Lifted(pq"$each($x @ _)", List(preamble))
          }
        } else {
          val reason = s"Because no implicit value of type me.rexim.morganey.meta.Unliftable[$tpe] could be found!"
          val msg    = s"Couldn't unlift a lambda term to a value of type $tpe! ($reason)"
          c.abort(subpattern.pos, msg)
        }
      }

      subpattern match {
        case pq"$_: $tpt" =>
          val typedTpt = c.typecheck(tpt, c.TYPEmode)
          val tpe =
            if (!dotted) typedTpt.tpe
            else iterableT(typedTpt.tpe)
          unQuote(tpe)
        case _ =>
          unQuote(LambdaTermTpe)
      }
  }

  private def wrapAst(term: LambdaTerm): Tree = method match {
    /**
      * - create an expression, that yields `term` at runtime, by lifting the tree (containing holes)
      * - replace holes with appropriate expressions, which themselves have to be lifted, too
      */
    case TermName("apply") =>
      val lifted = liftComplexTerm(term)
      q"""
        ..${lifted.preamble}
        ${lifted.exp}
      """

    /**
      * - create a pattern, which allows to extract the values at the positions, which are marked by holes
      */
    case TermName("unapply") =>
      val ps = parts.indices.init
      val (ifp, elp) = parts match {
        case Nil =>
          c.abort(c.enclosingPosition, "Internal error: \"parts\" is empty.")
        case _ :: Nil =>
          (q"true", q"false")
        case _ =>
          val ys = ps map { i =>
            TermName(s"x$i")
          }
          (q"_root_.scala.Some((..$ys))", q"_root_.scala.None")
      }

      val lifted = liftComplexTerm(term)

      /*
       * Workaround to avoid the warning: patterns after a variable pattern cannot match (SLS 8.1.1)
       * (See http://alvinalexander.com/scala/scala-unreachable-code-due-to-variable-pattern-message)
       */
      val identName = TermName(c.freshName())
      q"""
        new {
          val $identName = true

          ..${lifted.preamble}

          def unapply(input: $LambdaTermTpe) = input match {
            case ${lifted.exp} if $identName => $ifp
            case _                           => $elp
          }
        }.unapply(..$args)
      """
  }

}
