package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.meta._

object TermOutputHelper {

  def smartShowTerm(term: LambdaTerm): String = {
    val (prefix, value) = decode(term)
    s"$prefix: $value"
  }

  private def decode(term: LambdaTerm): (String, String) =
    decoders
      .map(_(term))
      .find(_.isDefined)
      .flatten
      .getOrElse(("term", term.toString))

  private val decoders: Stream[Decoder[_]] =
    Stream(
      Decoder[Char]                     ("char", c => s"'$c'"),
      Decoder[Int]                      ("number", _.toString),
      Decoder[String]                   ("string", s => s"""\"$s\""""),
      Decoder[Seq[Int]]                 ("numbers", _.mkString("[", ",", "]")),
      Decoder[Seq[LambdaTerm]]          ("elements", _.map(decode(_)._2).mkString("[", ",", "]")),
      Decoder[(LambdaTerm, LambdaTerm)] ("pair", p => s"(${decode(p._1)._2},${decode(p._2)._2})")
    )

  private case class Decoder[T](prefix: String, transform: T => String)(implicit unT: Unliftable[T]) {
    def apply(term: LambdaTerm): Option[(String, String)] = unT.unapply(term).map(transform).map((prefix, _))
  }

}
