package report

import java.util.Date

import scala.collection.mutable


class StopWatch {
  var init: Long = 0
  var from: Long = 0
  var elapsed: Long = 0

  private var running: Int = 0

  import StopWatch._

  def nowMs = {
    val ctm = System.currentTimeMillis()
    if (init == 0) init = ctm
    ctm
  }

  def now = {
    val ctm = nowMs
    Instant(ctm - init, new Date(ctm))
  }

  def start() { if (running == 0) from = nowMs; running += 1 }
  def stop()  { running -= 1; if (running == 0) elapsed += (nowMs - from) }

  def timed[T](op: => T): T =
    try { start(); op } finally { stop() }
}


object StopWatch {

  case class Instant(offset: Long, time: Date) {
    override def toString: String = s"${time} [+${offset}]"
  }

  lazy val instance = new StopWatch

  class FactoryMap[K,V] extends mutable.HashMap[K,V] {
    override def apply(key: K): V = {
      val result = findEntry(key)
      if (result eq null) { val v = default(key); put(key, v); v }
      else result.value
    }
  }
  object factory extends FactoryMap[String, StopWatch] {
    override def default(key: String) = new StopWatch
  }
}


trait LazyTiming {
  protected def watchName: String = getClass.getSimpleName
  @transient
  protected lazy val watch: StopWatch = StopWatch.factory(watchName)

  def timed[T](op: => T): T = watch.timed(op)
}