package me.rexim.morganey

import java.util.{List => Jlist}

import jline.console.completer.Completer
import me.rexim.morganey.ast.MorganeyBinding

import scala.annotation.tailrec

class ReplAutocompletion(globalContext: () => List[MorganeyBinding]) extends Completer {

  override def complete(buffer: String, cursor: Int, candidates: Jlist[CharSequence]): Int = {
    val knownVariableNames = globalContext().map(_.variable.name)
    val (beforeCursor, _) = buffer splitAt cursor
    lazy val definitions = matchingDefinitions(beforeCursor, knownVariableNames)

    if (knownVariableNames.isEmpty) {
      // if there are no names, REPL can't autocomplete
      -1
    } else if (definitions.nonEmpty) {
      // if there are matching definitions, use them for autocompletion
      for (definition <- definitions) candidates.add(definition)
      0
    } else {
      // if there aren't any matching definitions
      if (buffer.trim.isEmpty) {
        // .. and nothing was typed in the REPL, yet:
        // autocomplete with all known variable names
        for (name <- knownVariableNames) candidates.add(name)
      } else {
        // .. and something was typed in the REPL, yet:
        // autocomplete with all variable names starting with it
        for (name <- knownVariableNames) {
          if (name.toLowerCase startsWith beforeCursor.toLowerCase) candidates.add(name)
        }
      }
      0
    }
  }

  private def matchingDefinitions(line: String, knownVariableNames: List[String]): List[String] =
    for {
      name <- knownVariableNames
      matchLength = countPrefixMatch(line, name)
      if matchLength > 0
      prefix = line.substring(0, line.length - matchLength)
    } yield prefix + name

  private def countPrefixMatch(line: String, definition: String): Int = {
    @tailrec
    def helper(xs: List[Char], ys: List[Char], acc: Int): Int = (xs, ys) match {
      case (xh :: xt, yh :: yt) =>
        if (xh == yh) helper(xt, yt, acc + 1)
        else acc
      case _ =>
        acc
    }

    def normalize(s: String) =
      s.toLowerCase.toList

    lastNameInLine(line)
        .map(ln => helper(normalize(definition), normalize(ln), 0))
        .getOrElse(0)
  }

  private val reversedName = "[a-zA-Z0-9]*[a-zA-Z]"

  private def lastNameInLine(line: String): Option[String] = {
    def stringMatches(n: Int): Option[String] =
      Option(line takeRight n).filter(_.matches(reversedName))

    def size(str: Option[String]): Int =
      str.map(_.length).getOrElse(0)

    val lengths = (0 to line.length).toStream
    lengths.map(stringMatches).maxBy(size)
  }

}
