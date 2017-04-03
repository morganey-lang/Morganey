package me.rexim.morganey.funtests

import scala.sys.process._
import java.io.File
import me.rexim.morganey.BuildInfo

trait MorganeyProcess {
  def morganey(args: String*): ProcessBuilder =
    // TODO(00beaa2c-945e-4df0-ad4f-3c38be47273c): improve usability of morganey program execution
    //
    // Specifying the full classpath and full entry classname just to
    // run a program is not really usable for the user perspective
    s"java -cp .${File.pathSeparator}./target/scala-2.11/morganey-assembly-${BuildInfo.version}.jar me.rexim.morganey.Main ${args.mkString(" ")}"
}
