package me.rexim.morganey

import java.io.File
import java.util.{List => Jlist}

import jline.console.completer.Completer
import me.rexim.morganey.Commands._
import me.rexim.morganey.ast._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.interpreter.ReplContext
import me.rexim.morganey.syntax.LambdaParser
import me.rexim.morganey.syntax.Language.identifier
import me.rexim.morganey.util._

import scala.util.Try

class ReplAutocompletion(globalContext: () => ReplContext) extends Completer {

  override def complete(buffer: String, cursor: Int, candidates: Jlist[CharSequence]): Int = {
    val knownVariableNames = globalContext().bindings.map(_.variable.name)
    val (beforeCursor, _) = buffer splitAt cursor
    lazy val definitions = matchingDefinitions(beforeCursor, knownVariableNames, cursor)

    def autoCompleteWith(xs: List[String]) = {
      for (elem <- xs) candidates.add(elem)
      if (candidates.isEmpty) -1 else 0
    }

    beforeCursor match {
      // if a command was typed in
      case IsCommand(cmds)                   => autoCompleteWith(cmds map (":" + _))
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
        autoCompleteWith(knownVariableNames filter (matches(_, beforeCursor)))
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
      case (xs :+ x, false)  => everythingIn(xs, matches(_, x))
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

  private object IsCommand {

    def unapply(line: String): Option[List[String]] = {
      val potentialCommand = parseCommand(line).map(_._1)
      val matchingCommands = potentialCommand map { p =>
        commands.keySet filter (_ startsWith p)
      }
      matchingCommands.map(_.toList)
    }

  }

  private def matches(definition: String, name: String) =
    definition.toLowerCase startsWith name.toLowerCase

  private def matchingDefinitions(line: String, knownVariableNames: List[String], cursor: Int): List[String] = {
    lazy val globalPrefix = line take cursor
    lazy val allNames     = knownVariableNames map (globalPrefix + _)
    val lastName = lastNameInLine(line)

    val filtered = lastName map { case (off, lname) =>
      val names  = knownVariableNames filter (matches(_, lname))
      val prefix = line take off
      names map (prefix + _)
    }

    // if no names match, autocomplete with all known names
    // e.g.: (|
    filtered.getOrElse(allNames)
  }

  private def lastNameInLine(line: String): Option[(Int, String)] = {
    def stringMatches(n: Int): Option[(Int, String)] =
      Option(line takeRight n)
        .filter(_.matches(identifier))
        .map(id => (line.length - n) -> id)

    def size(str: Option[(Int, String)]): Int =
      str.map { case (_, s) => s.length }.getOrElse(Int.MinValue)

    val lengths = (0 to line.length).toStream
    lengths.map(stringMatches).maxBy(size)
  }

}
