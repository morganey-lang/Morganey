package me.rexim.morganey

package object syntax {
  implicit class LambdaParseResultConversions[R](result: LambdaParser.ParseResult[R]) {
    def toTry: scala.util.Try[R] = result match {
      case LambdaParser.Success(result, _) => scala.util.Success(result)
      case res => scala.util.Failure(new LambdaParserException(res.toString))
    }

    def toOption: Option[R] = result match {
      case LambdaParser.Success(result, _) => Some(result)
      case res => None
    }
  }
}
