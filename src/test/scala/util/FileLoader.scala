package util

import scala.io.Source
import scala.util.Try

trait FileLoader {

  def tryReadTextFile(filename: String): Try[List[String]] = Try(readTextFile(filename))

  def readTextFile(filename: String): List[String] =
    using(Source.fromResource(filename)) {
      source => source.getLines.toList
    }

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

}
