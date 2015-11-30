package me.rexim

object Main {

  def betaReduction(expression: LambdaTerm) =
    expression match {
      case LambdaApplication(f, argument) => f(argument)
      case _ => expression
    }

  def main(args: Array[String]): Unit = ???
}
