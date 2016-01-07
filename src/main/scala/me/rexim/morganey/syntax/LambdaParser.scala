package me.rexim.morganey.syntax

import me.rexim.morganey.ast._

import scala.util.parsing.combinator._

object LambdaParser extends RegexParsers {

  def variable: Parser[LambdaVar] = {
    regex("[a-zA-Z0-9]+".r) ^^ {
      name => LambdaVar(name)
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
    variable | func | application

  def binding: Parser[MorganeyBinding] =
    variable ~ ":=" ~ term ^^ {
      case lambdaVar ~ _ ~ term => MorganeyBinding(lambdaVar, term)
    }

  def replCommand: Parser[MorganeyNode] = binding | term
}

