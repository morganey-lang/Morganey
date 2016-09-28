package me.rexim.morganey.funtests

import scala.sys.process._

trait MorganeyProcess {
  def morganey(args: String*): ProcessBuilder =
    s"java -jar ./target/scala-2.11/morganey.jar ${args.mkString(" ")}"
}
