package me.rexim.morganey.syntax

import me.rexim.morganey.ast._
import me.rexim.morganey.church.{ChurchNumberConverter, ChurchPairConverter}
import me.rexim.morganey.util._
import me.rexim.morganey.syntax.Language._

import scala.util.parsing.combinator._
import scala.language.postfixOps

object IntMatcher {
  def unapply(rawInt: String): Option[Int] =
    try {
      Some(Integer.parseInt(rawInt))
    } catch {
      case e: NumberFormatException => None
    }
}

object LambdaParser extends LambdaParser

class LambdaParser extends JavaTokenParsers with ImplicitConversions {

  /* comment-regex taken from: http://stackoverflow.com/a/5954831 */
  protected override val whiteSpace = whiteSpacePattern.r

  def variable: Parser[LambdaVar] =
    identifier.r ^^ { LambdaVar }

  def numericLiteral: Parser[LambdaTerm] =
    numberLiteral.r ^? ({
      case IntMatcher(x) => ChurchNumberConverter.encodeNumber(x)
    }, { (rawInt) => s"`$rawInt' is too big"})

  def characterLiteral: Parser[LambdaTerm] = (
    escapedCharLiteral.r ^^ { s =>
      ChurchNumberConverter.encodeNumber(escapeSequences(s charAt 2))
    }
    | symbolCharLiteral.r ^^ { s =>
      ChurchNumberConverter.encodeNumber(s charAt 1)
    }
  )

  def stringLiteralTerm: Parser[LambdaTerm] =
    stringLiteral ^^ { s =>
      ChurchPairConverter.encodeString(unquoteString(s))
    }

  private def lambda = lambdaLetter | lambdaSlash

  private def parenthesis[T](p: Parser[T]): Parser[T] =
    leftParenthesis ~> p <~ rightParenthesis

  def func: Parser[LambdaFunc] =
    parenthesis((lambda ~> variable) ~ (abstractionDot ~> term)) ^^ { LambdaFunc }

  def application: Parser[LambdaApp] =
    parenthesis(term ~ term) ^^ { LambdaApp }

  def term: Parser[LambdaTerm] =
    variable | numericLiteral | characterLiteral | stringLiteralTerm | func | application

  def binding: Parser[MorganeyBinding] =
    (variable <~ bindingAssign) ~ term ^^ { MorganeyBinding }

  def loading: Parser[MorganeyLoading] =
    loadKeyword ~> (modulePath.r ?) ^^ { MorganeyLoading }

  def replCommand: Parser[MorganeyNode] = loading | binding | term

  def script: Parser[List[MorganeyNode]] = rep(replCommand)

  def module: Parser[List[MorganeyNode]] = rep(loading | binding)
}
