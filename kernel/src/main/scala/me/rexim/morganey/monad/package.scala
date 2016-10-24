package me.rexim.morganey

import scala.util.Try

package object monad {
  /**
    * Returns 'None', if one of the Options in 'lst' is 'None',
    * otherwise the elements are collected in a 'Some'.
    */
  def sequence[T](lst: List[Option[T]]): Option[List[T]] =
    lst.foldRight(Option(List.empty[T])) {
      case (ele, acc) => acc.flatMap(lst => ele.map(_ :: lst))
    }

  def sequence[T](lst: List[Try[T]]): Try[List[T]] =
    lst.foldRight(Try(List.empty[T])) {
      case (ele, acc) => acc.flatMap(lst => ele.map(_ :: lst))
    }

  def sequenceRight[L, R](lst: List[Either[L, R]]): Either[L, List[R]] =
    lst.foldRight[Either[L, List[R]]](Right(List.empty[R])) {
      case (ele, acc) => acc.right.flatMap(lst => ele.right.map(_ :: lst))
    }
}
