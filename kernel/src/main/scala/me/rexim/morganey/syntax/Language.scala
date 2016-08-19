package me.rexim.morganey.syntax

object Language {

  val identifier         = "[a-zA-Z][a-zA-Z0-9]*"

  val numberLiteral      = "[0-9]+"

  val escapedCharLiteral = """'\\[\\'"bfnrt]'"""

  val symbolCharLiteral  = "'[\u0020-\u00B0]'"

  val modulePath         = "[a-zA-Z][a-zA-Z0-9.]*"

  /** For handling escape sequences, which are currently supported as `characterLiteral` */
  val escapeSequences =
    Map[Char, Char](
      'b' -> '\b',
      'f' -> '\f',
      'n' -> '\n',
      'r' -> '\r',
      't' -> '\t'
    ) withDefault identity

  val lambdaLetter       = "λ"

  val lambdaSlash        = "\\"

  val loadKeyword        = "load"

  val bindingAssign      = ":="

  val abstractionDot     = "."

  val comma              = ","

  val whiteSpacePattern  = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+"""

  val leftParenthesis    = "("

  val rightParenthesis   = ")"

  val leftBracket        = "["

  val rightBracket       = "]"

  val rangeOperator      = ".."

}
