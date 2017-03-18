package me.rexim.morganey

import java.io.{File, FileInputStream, InputStreamReader, Reader}
import java.net.URL
import java.nio.charset.StandardCharsets.UTF_8
import java.util.regex.Pattern
import scala.util._

package object util {
  // TODO(#276): move validRegex to Commands
  def validRegex(regex: String): Option[String => Boolean] =
    Try {
      val pattern = Pattern.compile(regex)
      s: String => pattern.matcher(s).matches()
    }.toOption
}
