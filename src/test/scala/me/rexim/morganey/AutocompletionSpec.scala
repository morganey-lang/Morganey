package me.rexim.morganey

import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.ast.MorganeyBinding
import me.rexim.morganey.helpers.TestTerms
import scala.collection.JavaConversions._
import org.scalatest._

class AutocompletionSpec extends FlatSpec with Matchers with TestTerms  {

  def autocomplete(line: String, cursor: Int, knownNames: List[String]): Set[CharSequence] = {
    val id = I(lvar("x"))
    val fakeBindings = knownNames.map(name => MorganeyBinding(lvar(name), id))
    val autocompleter = new ReplAutocompletion(() => fakeBindings)
    val jlist = new java.util.ArrayList[CharSequence]()
    autocompleter.complete(line, cursor, jlist)
    jlist.toSet
  }

  private val manyBindings            = List("SUCC", "PRED", "MULT", "PLUS")
  // toplevel modules of morganey, add a "." after names of directories
  private val topLevelMorganeyModules = Set("prelude", "math.")
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

}
