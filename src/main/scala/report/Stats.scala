package report

import java.io.{FileOutputStream, OutputStreamWriter}

import scala.collection.mutable
import synthesis.search.rewrites.PatternRewriteRule


class Stats {
  import Stats._

  val ruleUsage = new RuleUsage

  def dumpToFile(filename: String = "stats.out") {
    val out = new OutputStreamWriter(new FileOutputStream(filename))
    try {
      out.write(s"${ruleUsage.dump}\n\n")
      out.write(s"${Timing.dump}\n\n■ ${Timing.end}")
    }
    finally { out.close() }
  }

  def dumpOnExit(filename: String = "stats.out"): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        dumpToFile(filename)
      }
    })
  }
}


object Stats {

  class Histogram[T] extends mutable.HashMap[T, Int] {
    def inc(key: T): Unit = put(key, this(key) + 1)
    override def default(key: T) = 0
  }

  class RuleUsage {
    import RuleUsage._
    val counts = new mutable.HashMap[PatternRewriteRule, Entry] {
      override def default(key: PatternRewriteRule) = new RuleUsage.Entry
      override def apply(key: PatternRewriteRule) = get(key) match {
        case Some(v) => v
        case None => val v = default(key); put(key, v); v
      }
    }
    def inc(rule: PatternRewriteRule, numEdges: Int) {
      val e = counts(rule)
      e.times += 1; e.edges += numEdges
    }

    def dump =
      new Table(counts.map { case (rule, e) => List(rule.termString, e.times, e.edges) } .toList)
        .withTotals(Seq(1, 2), Table.sumInts[Int])
  }

  object RuleUsage {
    class Entry { var times = 0; var edges = 0 }
  }

  object Timing {
    def dump =
      new Table((for ((k, v) <- StopWatch.factory) yield List(k, v.elapsed)).toList)
        .withTotals(Seq(1), Table.sumInts[Long])

    def end = s"finished  @  ${StopWatch.instance.now}"
  }

  /**
    * Formatting of table data for textual output
    * @param data list of row data
    * @tparam T cell data type
    */
  class Table[T](val data: List[List[Option[T]]]) {
    def this(data: List[List[T]])(implicit dummy: DummyImplicit) = this(data.map(_.map(Some(_))))

    override def toString = if (data.isEmpty) "" else {
      val ncols = data.map(_.length).max
      val str = data.map(_.map(formatCell).padTo(ncols, ""))
      val colWidths = (0 until ncols).map(i => str.map(_(i).length).max)
      str.map(_.zip(colWidths)
        .map { case (text, width) => text.padTo(width, ' ') } .mkString("  ")
      ).mkString("\n")
    }
    private def formatCell(v: Option[T]) = v.map(toStringSafe).getOrElse("")
    private def toStringSafe(o: Any) = if (o == null) "(null)" else o.toString

    def withTotals(cols: Seq[Int], aggregate: Seq[T] => T) = if (cols.isEmpty) this else {
      val sums = cols.map(i => i -> aggregate(data.map(_(i)).collect { case Some(v) => v })).toMap
      val row = (0 to cols.max).map(sums.get).toList
      new Table[T](data :+ row)
    }
  }

  object Table {
    def sumInts[T](d: Seq[Any])(implicit num: Numeric[T]) = d.map(_.asInstanceOf[T]).sum
  }

  lazy val instance = new Stats
}
