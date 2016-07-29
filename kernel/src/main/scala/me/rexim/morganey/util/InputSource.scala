package me.rexim.morganey.util

import java.io.Reader

import scala.language.implicitConversions

object InputSource {

  implicit def fromString(string: String): InputSource =
    StringSource(string)

  implicit def fromReader(reader: Reader): InputSource =
    ReaderSource(reader)

}

sealed trait InputSource

case class StringSource(string: String) extends InputSource

case class ReaderSource(reader: Reader) extends InputSource
