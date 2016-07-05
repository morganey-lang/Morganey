package me.rexim.morganey.syntax

import me.rexim.morganey.ast._
import me.rexim.morganey.church.{ChurchNumberConverter, ChurchPairConverter}
import me.rexim.morganey.util._

import scala.util.parsing.combinator._

object IntMatcher {
  def unapply(rawInt: String): Option[Int] =
    try {
      Some(Integer.parseInt(rawInt))
    } catch {
      case e: NumberFormatException => None
    }
}

object LambdaParser extends LambdaParser

class LambdaParser extends JavaTokenParsers {

  def variable: Parser[LambdaVar] = {
    "[a-zA-Z][a-zA-Z0-9]*".r ^^ {
      name => LambdaVar(name)
    }
  }

  def numericLiteral: Parser[LambdaTerm] =
    "[0-9]+".r ^? ({
      case IntMatcher(x) => ChurchNumberConverter.encodeNumber(x)
    }, { (rawInt) => s"`$rawInt' is too big"})

  def characterLiteral: Parser[LambdaTerm] = (
    """'\\[\\'"bfnrt]'""".r ^^ { s =>
      ChurchNumberConverter.encodeNumber(escapeSequences(s charAt 2))
    }
  | "'[\u0020-\u00B0]'".r ^^ { s =>
      ChurchNumberConverter.encodeNumber(s charAt 1)
    }
  )

  /** For handling escape sequences, which are currently supported as `characterLiteral` */
  private def escapeSequences =
    Map[Char, Char](
      'b' -> '\b',
      'f' -> '\f',
      'n' -> '\n',
      'r' -> '\r',
      't' -> '\t'
    ) withDefault identity

  def stringLiteralTerm: Parser[LambdaTerm] =
    stringLiteral ^^ {
      case s => {
        ChurchPairConverter.encodeString(unquoteString(s)).get
      }
    }

  def func: Parser[LambdaFunc] = {
    "(" ~ ("λ" | "\\") ~ variable ~ "." ~ term ~ ")" ^^ {
      case "(" ~ _ ~ v ~ "." ~ t ~ ")" => LambdaFunc(v, t)
    }
  }

  def application: Parser[LambdaApp] = {
    "(" ~ term ~ term ~ ")" ^^ {
      case "(" ~ t1 ~ t2 ~ ")" => LambdaApp(t1, t2)
    }
  }

  def term: Parser[LambdaTerm] =
    variable | numericLiteral | characterLiteral | stringLiteralTerm | func | application

  def binding: Parser[MorganeyBinding] =
    variable ~ ":=" ~ term ^^ {
      case lambdaVar ~ _ ~ term => MorganeyBinding(lambdaVar, term)
    }

  def replCommand: Parser[MorganeyNode] = binding | term

  def script: Parser[List[MorganeyNode]] = rep(replCommand)
}

