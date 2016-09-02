package me.rexim.morganey

import java.io.{File, FileInputStream, InputStreamReader, Reader}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.regex.Pattern

import scala.util._
import me.rexim.morganey.syntax.{LambdaParser, LambdaParserException}

package object util {

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

  def validRegex(regex: String): Option[String => Boolean] =
    Try {
      val pattern = Pattern.compile(regex)
      s: String => pattern.matcher(s).matches()
    }.toOption

  def unquoteString(s: String): String =
    if (s.isEmpty) s else s(0) match {
      case '"' => unquoteString(s.tail)
      case '\\' => s(1) match {
        case 'b' => '\b' + unquoteString(s.drop(2))
        case 'f' => '\f' + unquoteString(s.drop(2))
        case 'n' => '\n' + unquoteString(s.drop(2))
        case 'r' => '\r' + unquoteString(s.drop(2))
        case 't' => '\t' + unquoteString(s.drop(2))
        case '"' => '"'  + unquoteString(s.drop(2))
        case 'u' => Integer.parseInt(s.drop(2).take(4), 16).toChar + unquoteString(s.drop(6))
        case c => c + unquoteString(s.drop(2))
      }
      case c => c + unquoteString(s.tail)
    }

  def reader(path: String): Try[Reader] = reader(new File(path))

  def reader(file: File): Try[Reader] = {
    val inputStream = Try(new FileInputStream(file))
    inputStream.map(new InputStreamReader(_, UTF_8))
  }

  def withReader[T](path: String)(f: Reader => Try[T]): Try[T] =
    withReader(new File(path))(f)

  def withReader[T](file: File)(f: Reader => Try[T]): Try[T] =
    reader(file).flatMap { reader =>
      val result = f(reader)
      reader.close()
      result
    }

  implicit class ParserOps[T <: LambdaParser](parser: T) {
    def parseWith[R](input: InputSource, f: T => parser.Parser[R]): Try[R] = {
      val production = f(parser)
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
