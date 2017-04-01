package me.rexim.morganey.funtests

import scala.sys.process._
import me.rexim.morganey.BuildInfo

trait MorganeyProcess {
  def morganey(args: String*): ProcessBuilder =
    s"java -cp .:./target/scala-2.11/morganey-assembly-${BuildInfo.version}.jar me.rexim.morganey.Main ${args.mkString(" ")}"
}
