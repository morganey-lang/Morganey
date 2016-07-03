package me.rexim.morganey.meta

import me.rexim.morganey.ast._
import me.rexim.morganey.syntax.LambdaParser

/**
 * Parser, which allows dollar signs as the start of identifiers,
 * as special-marker for holes, which will be used during quotation
 * of syntax nodes into another nodes.
 */
object MetaParser extends LambdaParser {

  override def variable: Parser[LambdaVar] =
    "\\$?[a-zA-Z][a-zA-Z0-9]*".r ^^ LambdaVar

}
