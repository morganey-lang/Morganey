package me.rexim.morganey.autocompletion.extractors

import me.rexim.morganey.ast._
import me.rexim.morganey.syntax._
import me.rexim.morganey.util._

object LoadStatement {
  private val zeroLoad = (Nil, false)

  private def pathInformation(path: String) = {
    val pathElements = path.split("\\.", -1).toList
    val endsWithDot  = pathElements.lastOption.exists(_.isEmpty)
    val realPathElements =
      if (endsWithDot) pathElements.init
      else pathElements
    (realPathElements, endsWithDot)
  }

  private def handleLoading(load: MorganeyLoading) =
    load.modulePath map pathInformation getOrElse zeroLoad

  def unapply(line: String): Option[(List[String], Boolean)] = {
    val parseResult = LambdaParser.parseAll(LambdaParser.loading, line).toOption
    parseResult.map(handleLoading)
  }
}
