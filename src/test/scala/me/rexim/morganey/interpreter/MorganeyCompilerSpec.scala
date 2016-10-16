package me.rexim.morganey.interpreter

import java.io.File

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.module.ModuleFinder
import me.rexim.morganey.meta._
import org.scalatest._

import scala.util.Success

class MorganeyCompilerSpec extends FlatSpec with Matchers {
  val moduleFinder = new ModuleFinder(List(new File("./std/src/main/resources/std/")))

  val programBindings = List(
    MorganeyBinding(m"x", m"\\x. x"),
    MorganeyBinding(m"y", m"\\y. y"),
    MorganeyBinding(m"z", m"\\z. z")
  )

  val programBody = m"x"

  val programEntry = MorganeyBinding(lvar("main"), programBody)

  val rawProgram = programEntry :: programBindings

  val programInput = "khooy".toStream

  "Compiler" should "compile a correct program to a reducible term" in {
    val Right(expectedProgram) = programBody.addBindings(MorganeyBinding(LambdaVar("input"), LambdaInput(() => programInput)) :: programBindings)
    MorganeyCompiler.compileProgram(() => programInput)(rawProgram) should be (Success(expectedProgram))
  }

  "Compiler" should "fail an incorrect program" in {
    MorganeyCompiler.compileProgram(() =>programInput)(programBindings).isFailure should be (true)
  }

  "Compiler" should "not interpret empty loading nodes and just ignore them" in {
    MorganeyCompiler.interpretNode(MorganeyLoading(None), moduleFinder, Set()) should be (Success(List()))
  }

  "Compiler" should "should returns bindings as is during their interpretation as a node" in {
    val binding = MorganeyBinding(lvar("khooy"), lvar("khooy"))
    MorganeyCompiler.interpretNode(binding, moduleFinder, Set()) should be (Success(List(binding)))
  }
}
