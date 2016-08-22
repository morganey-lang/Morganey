package me.rexim.morganey.interpreter

import java.io.File

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.module.ModuleFinder
import org.scalatest._

import scala.util.Success

class MorganeyExecutorSpecs extends FlatSpec with Matchers {
  val moduleFinder = new ModuleFinder(List(new File("./std/")))

  val programBindings = List(
    MorganeyBinding(lvar("x"), lvar("x")),
    MorganeyBinding(lvar("y"), lvar("y")),
    MorganeyBinding(lvar("z"), lvar("z"))
  )

  val programEntry = MorganeyBinding(lvar("main"), lvar("main"))

  val rawProgram = programEntry :: programBindings

  val programInput = "khooy".toStream

  "Executor" should "compile a correct program to a reducible term" in {
    val expectedProgram = lapp(lvar("main"), LambdaInput(programInput)).addBindings(programBindings)
    MorganeyExecutor.compileProgram(programInput)(rawProgram) should be (Success(expectedProgram))
  }

  "Executor" should "fail an incorrect program" in {
    MorganeyExecutor.compileProgram(programInput)(programBindings).isFailure should be (true)
  }

  "Executor" should "not interpret empty loading nodes and just ignore them" in {
    MorganeyExecutor.interpretNode(MorganeyLoading(None), moduleFinder) should be (Success(List()))
  }

  "Executor" should "should returns bindings as is during their interpretation as a node" in {
    val binding = MorganeyBinding(lvar("khooy"), lvar("khooy"))
    MorganeyExecutor.interpretNode(binding, moduleFinder) should be (Success(List(binding)))
  }
}
