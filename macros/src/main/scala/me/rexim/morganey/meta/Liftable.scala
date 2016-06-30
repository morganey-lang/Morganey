package me.rexim.morganey.meta

import scala.language.higherKinds

import me.rexim.morganey.ast.LambdaTerm
import me.rexim.morganey.church.ChurchNumberConverter.encodeNumber
import me.rexim.morganey.church.ChurchPairConverter.{encodeList, encodePair}

trait Liftable[T] extends (T => LambdaTerm) {
  def apply(x: T): LambdaTerm
}

object Liftable {

  def apply[T](f: T => LambdaTerm): Liftable[T] =
    new Liftable[T] {
      def apply(x: T): LambdaTerm = f(x)
    }

  implicit val liftInt  = Liftable[Int](encodeNumber)
  implicit val liftChar = Liftable[Char](c => encodeNumber(c.toInt))

  implicit def liftPair[A, B](implicit liftA: Liftable[A], liftB: Liftable[B]): Liftable[(A, B)] =
    Liftable[(A, B)] {
      case (a, b) => encodePair( (liftA(a), liftB(b)) )
    }

  implicit def liftColl[X, CC[X] <: Traversable[X], A](implicit lift: Liftable[A]): Liftable[CC[A]] =
    Liftable[CC[A]] { xs =>
      val ys = xs map lift
      encodeList(ys.toList).getOrElse(
        sys.error("Can't transform empty list into lambda terms!")
      )
    }
}
