package me.rexim.morganey

import java.io.{File, FileInputStream, InputStreamReader, Reader}
import java.net.URL
import java.nio.charset.StandardCharsets.UTF_8
import scala.util._

package object reader {
  def reader(path: String): Try[Reader] = reader(new File(path))

  def reader(file: File): Try[Reader] = {
    val inputStream = Try(new FileInputStream(file))
    inputStream.map(new InputStreamReader(_, UTF_8))
  }

  def reader(url: URL): Try[Reader] = {
    Try(new InputStreamReader(url.openStream(), UTF_8))
  }

  def withReader[T](path: String)(f: Reader => Try[T]): Try[T] =
    withReader(new File(path))(f)

  def withReader[T](url: URL)(f: Reader => Try[T]): Try[T] =
    reader(url).flatMap { reader =>
      val result = f(reader)
      reader.close()
      result
    }

  def withReader[T](file: File)(f: Reader => Try[T]): Try[T] =
    reader(file).flatMap { reader =>
      val result = f(reader)
      reader.close()
      result
    }
}
