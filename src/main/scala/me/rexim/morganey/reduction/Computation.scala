package me.rexim.morganey.reduction

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

trait Computation[T] { self =>
  def future: Future[T]
  def cancel(): Unit

  def map[U](f: T => U): Computation[U] = new Computation[U] {
    override def cancel(): Unit = self.cancel()
    override def future: Future[U] = self.future.map(f)
  }

  def flatMap[U](f: T => Computation[U]): Computation[U] = new Computation[U] {
    val next = self.future.map(f)

    override def future: Future[U] = next.flatMap(_.future)
    override def cancel(): Unit = next.foreach(_.cancel())
  }

  def foreach(f: T => Unit): Unit = future.foreach(f)
}

object Computation {
  def apply[T](x: => T): Computation[T] = fromFuture(Future(x))

  def fromFuture[T](f: Future[T]): Computation[T] = new Computation[T] {
    override def cancel(): Unit = ()
    override def future: Future[T] = f
  }

  def failed[T](message: String, exception: String => Throwable): Computation[T] =
    fromFuture(Future.failed(exception(message)))
}
