package me.rexim.morganey.interpreter

import java.io.File

import me.rexim.morganey.ast._
import me.rexim.morganey.ast.LambdaTermHelpers._
import me.rexim.morganey.module.ModuleIndex
import me.rexim.morganey.meta._
import org.scalatest._

import scala.util.Success

class MorganeyCompilerSpec extends FlatSpec with Matchers {
  val programBindings = List(
    MorganeyBinding(m"x", m"\\x. x"),
    MorganeyBinding(m"y", m"\\y. y"),
    MorganeyBinding(m"z", m"\\z. z")
  )

  val programBody = m"x"

  val programEntry = MorganeyBinding(lvar("main"), programBody)

  val rawProgram = programEntry :: programBindings

  val programInput = "khooy".toStream

  behavior of "Morganey Compiler"

  it should "compile a correct program to a reducible term" in {
    val Right(expectedProgram) = programBody.addBindings(MorganeyBinding(LambdaVar("input"), LambdaInput(() => programInput)) :: programBindings)
    MorganeyCompiler.compileProgram(() => programInput)(rawProgram) should be (Success(expectedProgram))
  }

  it should "fail an incorrect program" in {
    MorganeyCompiler.compileProgram(() =>programInput)(programBindings).isFailure should be (true)
  }
}
