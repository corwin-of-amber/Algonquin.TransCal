package report

import java.util.Date


class StopWatch {

  var start: Long = 0

  import StopWatch._

  def now = {
    val ctm = System.currentTimeMillis()
    if (start == 0) start = ctm
    Instant(ctm - start, new Date(ctm))
  }

}


object StopWatch {

  case class Instant(offset: Long, time: Date) {
    override def toString: String = s"${time} [+${offset}]"
  }

  lazy val instance = new StopWatch

}