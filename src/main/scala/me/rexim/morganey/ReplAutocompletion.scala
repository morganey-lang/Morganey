package me.rexim.morganey

import java.io.File
import java.util.{List => Jlist}

import jline.console.completer.Completer
import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.interpreter.InterpreterContext
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.syntax.Language.identifier
import me.rexim.morganey.util._

import scala.annotation.tailrec
import scala.util.Try

class ReplAutocompletion(globalContext: () => InterpreterContext) extends Completer {

  override def complete(buffer: String, cursor: Int, candidates: Jlist[CharSequence]): Int = {
    val knownVariableNames = globalContext().bindings.map(_.variable.name)
    val (beforeCursor, _) = buffer splitAt cursor
    lazy val definitions = matchingDefinitions(beforeCursor, knownVariableNames)

    def autoCompleteWith(xs: List[String]) = {
      for (elem <- xs) candidates.add(elem)
      if (candidates.isEmpty) -1 else 0
    }

    beforeCursor match {
      // if a load statement was typed in (potential partially)
      case LoadStatement(parts, endsWithDot) =>
        val autocompletedModules = autocompleteLoadStatement(parts, endsWithDot)
        autoCompleteWith(autocompletedModules map (m => s"load $m"))
      // if there are no names, REPL can't autocomplete
      case _ if knownVariableNames.isEmpty   => -1
      // if there are definitions matching the users input, use them for completion
      case _ if definitions.nonEmpty         => autoCompleteWith(definitions)
      // if nothing was typed into the repl, autocomplete with all known names
      case _ if buffer.trim.isEmpty          => autoCompleteWith(knownVariableNames)
      // if something was typed into the repl, autocomplete with all names starting with the input text
      case _                                 =>
        autoCompleteWith(knownVariableNames filter (_.toLowerCase startsWith beforeCursor.toLowerCase))
    }
  }

  private def autocompleteLoadStatement(parts: List[String], endsWithDot: Boolean): List[String] = {
    import ModuleFinder._

    val moduleFinder = this.globalContext().moduleFinder

    def validMorganeyElement(f: File) =
      f.isDirectory || isMorganeyModule(f)

    def topLevelDefinitions() =
      moduleFinder.paths.flatMap(_.listFiles).filter(validMorganeyElement)

    // List(root-file-of-module-path, module-file or directory)
    def findAllModulesIn(path: String): List[(File, File)] =
      Try(
        moduleFinder.paths.toStream
          .map { f => (f, new File(f, loadPathToRelativeFile(path))) }
          .filter { case (_, f) => f.exists() }
          .flatMap { case (root, f) => f.listFiles() map (root -> _) }
          .filter { case (_, f) => validMorganeyElement(f) }
          .distinct
          .toList
      ).toOption.getOrElse(Nil)

    def stripExtensionIfModuleFile(f: File): File =
      if (isMorganeyModule(f)) new File(f.getParent, f.getName.replaceAll(s".$fileExtension", ""))
      else f

    def everythingIn(path: List[String], fileNameFilter: String => Boolean = _ => true) =
      findAllModulesIn(path.mkString("."))
        .map { case (root, f) => (root, stripExtensionIfModuleFile(f)) }
        .filter { case (root, f) => fileNameFilter(f.getName) }
        .map(relativize)
        .map(_.replace('/', File.separatorChar))
        .map(relativeFileToLoadPath)

    def relativize(baseAndFile: (File, File)): String = {
      val (base, file) = baseAndFile
      base.toURI().relativize(file.toURI()).getPath()
    }

    def moduleName(file: File): String = {
      val rawName = stripExtensionIfModuleFile(file).getName
      val suffix = if (file.isDirectory) "." else ""
      s"$rawName$suffix"
    }

    (parts, endsWithDot) match {
      // load .|
      case (Nil, true)       => Nil
      // load a.b.|
      case (xs, true)        => everythingIn(xs)
      // load |
      case (Nil, false)      => topLevelDefinitions().map(moduleName)
      // load math.ari|
      case (xs :+ x, false)  => everythingIn(xs, _.toLowerCase startsWith x.toLowerCase)
    }
  }

  private object LoadStatement {
    val zeroLoad = (Nil, false)

    def pathInformation(path: String) =
      if (path.isEmpty) {
        zeroLoad
      } else if (path == ".") {
        (Nil, true)
      } else {
        val pathElements = path.split("\\.", -1).toList
        val endsWithDot  = pathElements.lastOption.exists(_.isEmpty)
        val realPathElements =
          if (endsWithDot) pathElements.init
          else pathElements
        (realPathElements, endsWithDot)
      }

    def handleLoading(load: MorganeyLoading) =
      load.modulePath map pathInformation getOrElse zeroLoad

    def unapply(line: String): Option[(List[String], Boolean)] = {
      val parseResult = LambdaParser.parseWith(line, _.loading).toOption
      parseResult.map(handleLoading)
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

  private def lastNameInLine(line: String): Option[String] = {
    def stringMatches(n: Int): Option[String] =
      Option(line takeRight n).filter(_.matches(identifier))

    def size(str: Option[String]): Int =
      str.map(_.length).getOrElse(Int.MinValue)

    val lengths = (0 to line.length).toStream
    lengths.map(stringMatches).maxBy(size)
  }

}
