package me.rexim.morganey.interpreter

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.meta._

import scala.annotation.tailrec

object TermOutputHelper {

  def smartShowTerm(term: LambdaTerm): String = {
    @tailrec
    def decode(decoders: List[Decoder[_]]): String = decoders match {
      case hd :: tl => hd(term) match {
        case Some(result) => result
        case None         => decode(tl)
      }
      case Nil => idDecoder(term).get // `idDecoder` won't ever return `None`
    }
    decode(decoders)
  }

  private implicit val idDecoder = Decoder[LambdaTerm](t => s"term: $t")

  private val decoders: List[Decoder[_]] =
    List(
      Decoder[Char]     (c  => s"char: '$c'"),
      Decoder[Int]      (n  => s"number: $n"),
      Decoder[String]   (s  => s"""string: \"$s\""""),
      Decoder[Seq[Int]] (xs => s"numbers: ${xs.mkString("[", ",", "]")}")
    )

  private case class Decoder[T](transform: T => String)(implicit unT: Unliftable[T]) {
    def apply(term: LambdaTerm): Option[String] = unT.unapply(term).map(transform)
  }

}
