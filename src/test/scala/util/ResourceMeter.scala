package util

import org.openjdk.jol.info.GraphLayout

import scala.concurrent.duration.{Duration, NANOSECONDS}

trait ResourceMeter {

  def measureTime[T](func: => T): (T, Duration) = {
    val start = System.nanoTime()
    val result = func
    val end = System.nanoTime()
    (result, Duration((end - start), NANOSECONDS))
  }

  def size(o: Object) : Long = GraphLayout.parseInstance(o).totalSize()

}
