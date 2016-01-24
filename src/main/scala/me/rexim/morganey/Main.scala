package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast._
import me.rexim.morganey.church.ChurchPairConverter
import me.rexim.morganey.syntax.LambdaParser

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main {

  def startRepl() = {
    var globalContext = List[MorganeyBinding]()
    val con = new ConsoleReader()
    con.setPrompt("> ")

    while (true) {
      val line = con.readLine()
      val termParseResult = LambdaParser.parse(LambdaParser.replCommand, line)

      if (termParseResult.successful) {
        termParseResult.get match {
          case MorganeyBinding(variable, term) => {
            globalContext = MorganeyBinding(variable, term.addContext(globalContext).normalOrder()) :: globalContext
            con.println("Bound " + variable.name)
          }

          case term : LambdaTerm => {
            val result = term.addContext(globalContext).normalOrder()
            con.println(ReplHelper.smartPrintTerm(result))
          }
        }
      } else {
        con.println(termParseResult.toString)
      }
    }
  }

  def interpretOneMorganeyNode(node: MorganeyNode, context: List[MorganeyBinding]): List[MorganeyBinding] = {
    node match {
      case MorganeyBinding(variable, term) =>
        MorganeyBinding(variable, term.addContext(context).normalOrder()) :: context

      case term : LambdaTerm => {
        val result = term.addContext(context).normalOrder()
        println(ReplHelper.smartPrintTerm(result))
        context
      }
    }
  }

  def interpertMorganeyNodes(nodes: List[MorganeyNode], context: List[MorganeyBinding]): List[MorganeyBinding] = {
    nodes.foldLeft(context) {
      case (acc, node) => interpretOneMorganeyNode(node, acc)
    }
  }

  def interpretFile(fileName: String, context: List[MorganeyBinding]) : Try[List[MorganeyBinding]] = {
    Try(Source.fromFile(fileName).mkString)
      .map (LambdaParser.parseAll(LambdaParser.script, _))
      .flatMap {
        case parsedCode => parsedCode
            .map(Success(_))
            .getOrElse(Failure(new IllegalArgumentException(s"$fileName ${parsedCode.toString}")))
      }
      .map (interpertMorganeyNodes(_, context))

  }

  def main(args: Array[String]) = {
    if (args.isEmpty) {
      startRepl()
    } else {
      args.toStream.foldLeft[Try[List[MorganeyBinding]]](Success(List())) {
        case (context, fileName) => context.flatMap(interpretFile(fileName, _))
      } match {
        case Failure(e) => println(s"[ERROR] ${e.getMessage}")
        case _ =>
      }
    }
  }
}
