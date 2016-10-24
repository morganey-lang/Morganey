package me.rexim.morganey

import java.io.{File, FileInputStream, InputStreamReader, Reader}
import java.net.URL
import java.nio.charset.StandardCharsets.UTF_8
import java.util.regex.Pattern

import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}

import scala.util._

package object util {

  // TODO(#276): move validRegex to Commands
  def validRegex(regex: String): Option[String => Boolean] =
    Try {
      val pattern = Pattern.compile(regex)
      s: String => pattern.matcher(s).matches()
    }.toOption


  // TODO(#277): move reader stuff to me.rexim.morganey.reader
  def reader(path: String): Try[Reader] = reader(new File(path))

  def reader(file: File): Try[Reader] = {
    val inputStream = Try(new FileInputStream(file))
    inputStream.map(new InputStreamReader(_, UTF_8))
  }

  def reader(url: URL): Try[Reader] = {
    Try(new InputStreamReader(url.openStream()))
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

  // TODO(#278): Move ParseOps and InputSource closer to LambdaParser
  implicit class ParserOps[T <: LambdaParser](parser: T) {
    def parseWith[R](input: InputSource, f: T => parser.Parser[R]): Try[R] = {
      val production = f(parser)
      // TODO(#279): Move logic inside of ParseOps.parseWith to InputSource
      val result = input match {
        case StringSource(string) => parser.parseAll(production, string)
        case ReaderSource(reader) => parser.parseAll(production, reader)
      }
      handleResult(result)
    }

    private def handleResult[R](parseRes: parser.ParseResult[R]): Try[R] = parseRes match {
      case parser.Success(result, _) => Success(result)
      case res                       => Failure(new LambdaParserException(res.toString))
    }
  }
}
