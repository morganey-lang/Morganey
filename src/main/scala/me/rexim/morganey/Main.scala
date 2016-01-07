package me.rexim.morganey

import jline.console.ConsoleReader
import me.rexim.morganey.ast._
import me.rexim.morganey.church.ChurchPairConverter
import me.rexim.morganey.syntax.LambdaParser

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main {

  var globalContext = List[MorganeyBinding]()

  def startRepl() = {
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
            ChurchPairConverter.convertListOfNumbers(result) match {
              case Some(numbers) => if (numbers.forall(x => 32 <= x && x <= 176)) {
                con.println("string: " + numbers.map(_.toChar).mkString)
              } else {
                con.println("numbers: " + numbers.mkString(","))
              }
              case None => con.println("term: " + result.toString)
            }
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

        ChurchPairConverter.convertListOfNumbers(result) match {
          case Some(numbers) => if (numbers.forall(x => 32 <= x && x <= 176)) {
            println(numbers.map(_.toChar).mkString)
            context
          } else {
            println(numbers.mkString(","))
            context
          }

          case None => {
            println(result.toString)
            context
          }
        }
      }
    }
  }

  def interpertMorganeyNodes(nodes: List[MorganeyNode], context: List[MorganeyBinding]): List[MorganeyBinding] = {
    nodes.foldLeft(context) {
      case (acc, node) => interpretOneMorganeyNode(node, acc)
    }
  }

  def interpretFile(fileName: String) : Try[Unit] = {
    Try(Source.fromFile(fileName).mkString)
      .map (LambdaParser.parseAll(LambdaParser.script, _))
      .flatMap {
        case parsedCode => parsedCode
            .map(Success(_))
            .getOrElse(Failure(new IllegalArgumentException(s"$fileName ${parsedCode.toString}")))
      }
      .map {
        case nodes => globalContext = interpertMorganeyNodes(nodes, globalContext)
      }
  }

  def main(args: Array[String]) = {
    if (args.isEmpty) {
      startRepl()
    } else {
      args.toStream.map(interpretFile).dropWhile(_.isSuccess) match {
        case Failure(e) #:: _ => println(s"[ERROR] ${e.getMessage}")
        case _ =>
      }
    }
  }
}
