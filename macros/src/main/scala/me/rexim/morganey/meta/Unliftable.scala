package me.rexim.morganey.meta

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.church.ChurchNumberConverter.decodeNumber
import me.rexim.morganey.church.ChurchPairConverter.{decodeList, decodePair}
import me.rexim.morganey.util.sequence

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait Unliftable[T] extends (LambdaTerm => Option[T]) {
  def apply(t: LambdaTerm): Option[T]
}

trait DefaultUnliftableInstances {

  private val chars = Set(0 to 10 :_*).map(_.toChar)

  implicit val unliftInt = Unliftable[Int](decodeNumber)
  implicit val unliftChar = Unliftable[Char] { t =>
    decodeNumber(t).map(_.toChar) filter (chars.contains _)
  }

  implicit val unliftString = Unliftable[String] { s =>
    val unlift = implicitly[Unliftable[Seq[Char]]]
    unlift(s).map(_.mkString)
  }

  implicit def unliftPair[A, B](implicit una: Unliftable[A], unb: Unliftable[B]): Unliftable[(A, B)] =
    Unliftable[(A, B)] { t =>
      for {
        (x, y) <- decodePair(t)
        a      <- una(x)
        b      <- unb(y)
      } yield (a, b)
    }

  implicit def unliftColl[X, CC[X] <: Traversable[X], A]
                         (implicit unlift: Unliftable[A], cbf: CanBuildFrom[List[A], A, CC[A]]): Unliftable[CC[A]] =
    Unliftable[CC[A]] { t =>
      val decodeResults = decodeList(t).map(unlift)
      sequence(decodeResults) map { xs =>
        val bl = cbf()
        xs.foreach(bl += _)
        bl.result()
      }
    }

}

object Unliftable {

  def apply[T](f: LambdaTerm => Option[T]): Unliftable[T] =
    new Unliftable[T] {
      def apply(t: LambdaTerm): Option[T] = f(t)
    }

  def using[T](f: PartialFunction[LambdaTerm, T]): Unliftable[T] =
    new Unliftable[T] {
      def apply(t: LambdaTerm): Option[T] = f.lift(t)
    }

}
