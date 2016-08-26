package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.meta._

import scala.annotation.tailrec

object TermOutputHelper {

  def smartShowTerm(term: LambdaTerm, noPrefix: Boolean = false): String = {
    @tailrec
    def decode(decoders: List[Decoder[_]]): String = decoders match {
      case hd :: tl => hd(term) match {
        case Some(result) => if (noPrefix) result else s"${hd.prefix}: $result"
        case None         => decode(tl)
      }
      case Nil =>
        // `idDecoder` won't ever return `None`
        val result = idDecoder(term).get
        if (noPrefix) result else s"${idDecoder.prefix}: $result"
    }
    decode(decoders)
  }

  private val idDecoder = Decoder[LambdaTerm]("term", _.toString)

  private val decoders: List[Decoder[_]] =
    List(
      Decoder[Char]                     ("char", c => s"'$c'"),
      Decoder[Int]                      ("number", _.toString),
      Decoder[String]                   ("string", s => s"""\"$s\""""),
      Decoder[Seq[Int]]                 ("numbers", _.mkString("[", ",", "]")),
      Decoder[Seq[LambdaTerm]]          ("elements", _.map(smartShowTerm(_, true)).mkString("[", ",", "]"))
    )

  private case class Decoder[T](prefix: String, transform: T => String)(implicit unT: Unliftable[T]) {
    def apply(term: LambdaTerm): Option[String] = unT.unapply(term).map(transform)
  }

}
