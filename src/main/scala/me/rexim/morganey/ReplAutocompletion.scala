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

    def autoCompleteWith(xs: List[String]) = {
      for (elem <- xs) candidates.add(elem)
      if (candidates.isEmpty) -1 else 0
    }

    beforeCursor match {
      // if there are no names, REPL can't autocomplete
      case _ if knownVariableNames.isEmpty => -1
      // if there are definitions matching the users input, use them for completion
      case _ if definitions.nonEmpty       => autoCompleteWith(definitions)
      // if nothing was typed into the repl, autocomplete with all known names
      case _ if buffer.trim.isEmpty        => autoCompleteWith(knownVariableNames)
      // if something was typed into the repl, autocomplete with all names starting with the input text
      case _                               =>
        autoCompleteWith(knownVariableNames filter (_.toLowerCase startsWith beforeCursor.toLowerCase))
    }
  }

  private def matchingDefinitions(line: String, knownVariableNames: List[String]): List[String] =
    for {
      name <- knownVariableNames
      matchLength = countPrefixMatch(line, name)
      if matchLength > 0
      prefix = line dropRight matchLength
    } yield prefix + name

  private def countPrefixMatch(line: String, definition: String): Int = {
    @tailrec
    def countMatchingLetters(xs: List[Char], ys: List[Char], acc: Int): Int = (xs, ys) match {
      case (xh :: xt, yh :: yt) if xh == yh => countMatchingLetters(xt, yt, acc + 1)
      case _                                => acc
    }

    def normalize(s: String) = s.toLowerCase.toList
    val lineLowerCase = line.toLowerCase

    /**
      * Only autocomplete:
      * - If the user typed in a single incomplete name: autocomplete if the name,
      *    which was typed into the repl is a prefix of the name of a definition
      * - If the user typed in a single name, which is part of a complex term
      */
    def keepNamesAndComplexTerms(nameInLine: String): Boolean = nameInLine match {
      // input of repl is exactly `nameInLine`
      case s if s == lineLowerCase     => definition startsWith lineLowerCase
      // REPL has a complex term as input
      case s if s.length < line.length => true
      case _                           => false
    }

    lastNameInLine(line)
      .filter(keepNamesAndComplexTerms)
      .map(ln => countMatchingLetters(normalize(definition), normalize(ln), 0))
      .getOrElse(0)
  }

  private val namePattern = "[a-zA-Z][a-zA-Z0-9]*"

  private def lastNameInLine(line: String): Option[String] = {
    def stringMatches(n: Int): Option[String] =
      Option(line takeRight n).filter(_.matches(namePattern))

    def size(str: Option[String]): Int =
      str.map(_.length).getOrElse(Int.MinValue)

    val lengths = (0 to line.length).toStream
    lengths.map(stringMatches).maxBy(size)
  }

}
