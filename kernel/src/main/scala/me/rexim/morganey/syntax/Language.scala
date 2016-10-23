package me.rexim.morganey.syntax

import hiddenargs.{hidden, hiddenargs}

import scala.annotation.tailrec

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

  /* comment-regex taken from: http://stackoverflow.com/a/5954831 */
  val whiteSpacePattern  = """([ \t]|//.*|(?m)/\*(\*(?!/)|[^*]|[\n\r])*\*/)+"""

  val lineBreak          = "[\n\r]"

  val leftParenthesis    = "("

  val rightParenthesis   = ")"

  val leftBracket        = "["

  val rightBracket       = "]"

  val rangeOperator      = ".."

  @hiddenargs
  @tailrec
  def unquoteString(s: String, @hidden acc: String = ""): String =
    if (s.isEmpty) acc else s(0) match {
      case '"' => unquoteString(s.tail, acc)
      case '\\' => s(1) match {
        case 'b' => unquoteString(s.drop(2), acc + "\b")
        case 'f' => unquoteString(s.drop(2), acc + "\f")
        case 'n' => unquoteString(s.drop(2), acc + "\n")
        case 'r' => unquoteString(s.drop(2), acc + "\r")
        case 't' => unquoteString(s.drop(2), acc + "\t")
        case '"' => unquoteString(s.drop(2), acc + "\"")
        case 'u' => unquoteString(s.drop(6), acc + Integer.parseInt(s.slice(2, 6), 16).toChar.toString)
        case c   => unquoteString(s.drop(2), acc + c.toString)
      }
      case c => unquoteString(s.tail, acc + c.toString)
    }
}
