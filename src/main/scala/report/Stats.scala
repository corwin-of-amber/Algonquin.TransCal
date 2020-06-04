package report

import java.io.{FileOutputStream, OutputStreamWriter}

import scala.collection.mutable
import synthesis.search.rewrite.operators.RewriteRule


class Stats {
  import Stats._

  val ruleUsage = new RuleUsage

  def dumpToFile(filename: String = "stats.out") {
    val out = new OutputStreamWriter(new FileOutputStream(filename))
    try {
      out.write(ruleUsage.dump.toString())
    }
    finally { out.close() }
  }
}


object Stats {

  class Histogram[T] extends mutable.HashMap[T, Int] {
    def inc(key: T): Unit = put(key, this(key) + 1)
    override def default(key: T) = 0
  }

  class RuleUsage {
    import RuleUsage._
    val counts = new mutable.HashMap[RewriteRule, Entry] {
      override def default(key: RewriteRule) = new RuleUsage.Entry
      override def apply(key: RewriteRule) = get(key) match {
        case Some(v) => v
        case None => val v = default(key); put(key, v); v
      }
    }
    def inc(rule: RewriteRule, numEdges: Int) {
      val e = counts(rule)
      e.times += 1; e.edges += numEdges
    }

    def dump =
      new Table(counts.map { case (rule, e) => List(rule.termString, e.times, e.edges) } .toList)
        .withTotals(Seq(1, 2), _.map(_.asInstanceOf[Int]).sum)
  }

  object RuleUsage {
    class Entry { var times = 0; var edges = 0 }
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
    private def toStringSafe(o: Any) = if (o == null) "-null-" else o.toString

    def withTotals(cols: Seq[Int], aggregate: Seq[T] => T) = if (cols.isEmpty) this else {
      val sums = cols.map(i => i -> aggregate(data.map(_(i)).collect { case Some(v) => v })).toMap
      val row = (0 to cols.max).map(sums.get).toList
      new Table[T](data :+ row)
    }
  }

  lazy val instance = new Stats
}
