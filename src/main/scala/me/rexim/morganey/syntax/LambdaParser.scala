package me.rexim.morganey.syntax

import me.rexim.morganey.ast._
import me.rexim.morganey.church.ChurchNumberConverter

import scala.util.parsing.combinator._

object IntMatcher {
  def unapply(rawInt: String): Option[Int] =
    try {
      Some(Integer.parseInt(rawInt))
    } catch {
      case e: NumberFormatException => None
    }
}

object LambdaParser extends RegexParsers {

  def variable: Parser[LambdaVar] = {
    "[a-zA-Z][a-zA-Z0-9]*".r ^^ {
      name => LambdaVar(name)
    }
  }

  def numericLiteral: Parser[LambdaTerm] =
    "[0-9]+".r ^? ({
      case IntMatcher(x) => ChurchNumberConverter.encodeNumber(x)
    }, { (rawInt) => s"`$rawInt' is too big"})

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
    variable | numericLiteral | func | application

  def binding: Parser[MorganeyBinding] =
    variable ~ ":=" ~ term ^^ {
      case lambdaVar ~ _ ~ term => MorganeyBinding(lambdaVar, term)
    }

  def replCommand: Parser[MorganeyNode] = binding | term

  def script: Parser[List[MorganeyNode]] = rep(replCommand)
}

