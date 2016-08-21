package me.rexim.morganey.interpreter

import me.rexim.morganey.module.ModuleFinder
import org.scalatest._

class MorganeyInterpreterSpecs extends FlatSpec with Matchers {
  "Computation of an empty sequence of nodes" should "result in empty sequnce of computations" in {
    val moduleFinder = new ModuleFinder(List(new java.io.File("./std/")))
    val context = new InterpreterContext(List(), moduleFinder)
    MorganeyInterpreter.evalNodesComputation(List())(context).isEmpty should be (true)
  }
}
