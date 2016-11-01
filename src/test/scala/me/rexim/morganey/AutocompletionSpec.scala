package me.rexim.morganey

import java.io.File

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.ast.MorganeyBinding
import me.rexim.morganey.helpers.TestTerms
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.interpreter.ReplContext

import org.scalatest._

class AutocompletionSpec extends FlatSpec with Matchers with TestTerms  {

  def autocomplete(line: String, cursor: Int, knownNames: List[String]): Set[String] = {
    val id = I(lvar("x"))
    val fakeBindings = knownNames.map(name => MorganeyBinding(lvar(name), id))
    val moduleFinder = new ModuleFinder(List(new File("./src/test/resources/load-autocomplete/")))
    val context = ReplContext(fakeBindings, moduleFinder)
    ReplAutocompletion.complete(line, cursor, context).toSet
  }

  private val manyBindings            = List("SUCC", "PRED", "MULT", "PLUS")
  // toplevel modules of morganey, add a "." after names of directories
  private val topLevelMorganeyModules = Set("boolean", "math.", "list", "prelude")
  // modules in math.*, add a "." after names of directories
  private val mathMorganeyModules     = Set("arithmetic")

  "The repl" should "not autocomplete, if no bindings are known" in {
    autocomplete("", 0, List())   should be (Set())
    autocomplete("SU", 2, List()) should be (Set())
    autocomplete("t", 1, List())  should be (Set())
  }

  it should "not autocomplete, if no binding matches the typed text" in {
    autocomplete("t", 1, manyBindings)    should be (Set())
    autocomplete("te", 2, manyBindings)   should be (Set())
    autocomplete("term", 3, manyBindings) should be (Set())
  }

  it should "autocomplete a name, if the typed text matches the name" in {
    val bindings = List("SUCC")
    val expect = bindings.toSet
    autocomplete("s", 1, bindings)   should be (expect)
    autocomplete("sU", 2, bindings)  should be (expect)
    autocomplete("Su", 2, bindings)  should be (expect)
    autocomplete("suc", 3, bindings) should be (expect)
  }

  it should "autocomplete names, if the typed text matches them" in {
    val bindings = List("SUCC1", "SUCC2")
    val expect = bindings.toSet
    autocomplete("s", 1, bindings)   should be (expect)
    autocomplete("sU", 2, bindings)  should be (expect)
    autocomplete("Su", 2, bindings)  should be (expect)
    autocomplete("suc", 3, bindings) should be (expect)
  }

  it should "autocomplete names containing numbers, if the typed text matches them" in {
    val bindings = List("a123", "a987")
    autocomplete("a1", 2, bindings) should be (Set("a123"))
    autocomplete("a9", 2, bindings) should be (Set("a987"))
  }

  it should "autocomplete all known names, if nothing was typed into the repl" in {
    val expect = manyBindings.toSet
    autocomplete("", 0, manyBindings) should be (expect)
  }

  it should "autocomplete all known names in complex structures, too" in {
    autocomplete("(Su PLUS)", 3, manyBindings) should be (Set("(SUCC"))
    autocomplete("(\\x . m)", 7, manyBindings) should be (Set("(\\x . MULT"))
    autocomplete("(\\x . p)", 7, manyBindings) should be (Set("(\\x . PLUS", "(\\x . PRED"))
  }

  it should "autocomplete paths in load statements" in {
    def addLoad(s: String): String = s"load $s"

    autocomplete("load .", 6, List())        should be (Set() map addLoad)
    autocomplete("load ", 5, List())         should be (topLevelMorganeyModules map addLoad)
    autocomplete("load m", 6, List())        should be (topLevelMorganeyModules filter (_ startsWith "m") map addLoad)
    autocomplete("load math.", 12, List())   should be (mathMorganeyModules map ("math." + _) map addLoad)
    autocomplete("load math.ar", 12, List()) should be (Set("math.arithmetic") map addLoad)
  }

  it should "autocomplete commands" in {
    def addColon(s: String): String = ":" + s
    val allCommands = Commands.commands.values map (_.name) map addColon

    autocomplete(":exi", 4, List())  should be (Set("exit") map addColon)
    autocomplete(":unbi", 4, List()) should be (Set("unbind") map addColon)
    autocomplete(":", 1, List())     should be (allCommands.toSet)
  }

  it should "not remove a part of the text, which was typed in, during autocompletion" in {
    val bindings = List("isa", "isb")
    autocomplete("(isa", 4, bindings) should be (Set("(isa"))
    autocomplete("(isb", 4, bindings) should be (Set("(isb"))
    autocomplete("(is", 3, bindings)  should be (Set("(isa", "(isb"))
  }

}
