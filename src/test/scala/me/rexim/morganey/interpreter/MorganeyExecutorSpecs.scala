package me.rexim.morganey.interpreter

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.interpreter.MorganeyExecutor
import org.scalatest._

import scala.util.Success

class MorganeyExecutorSpecs extends FlatSpec with Matchers {
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
}
