package relentless

import java.io.FileWriter

import com.typesafe.scalalogging.LazyLogging
import relentless.matching.Encoding
import relentless.rewriting._
import syntax.AstSugar._

import scala.collection.mutable.ListBuffer

object Utils extends LazyLogging {

  def transposeAll[A](xss: Seq[Seq[A]], ph: A): List[List[A]] = {
    val buf = new ListBuffer[List[A]]
    var yss = xss
    while (!yss.forall(_.isEmpty)) {
      buf += (yss map (_.headOption getOrElse ph)).toList
      yss = yss map { case _::tail => tail case _ => Nil }
    }
    buf.toList
  }

  def ljust(s: String, n: Int): String = s ++ (" " * (n - s.length))

  def rtrim(s: String): String = s.replaceAll("\\s+$", "")

  def mkStringColumns[A](l: List[A], colWidth: Int): String =
    l map (_.toString) map (ljust(_, colWidth)) mkString " "

  /*
  def showem(matches: Seq[BaseRewriteEdge[Int]], trie: Trie[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding) {
    val except = Markers.all map (_.leaf) toSet;
    for (gm <- matches) {
      logger.info(s"${gm mkString " "}")//  [${gm map (enc.ntor <--) mkString "] ["}]");
      for (ln <- transposeAll(gm.toList drop 2 map (x => new Reconstructer(x, trie)(enc, except).toList), B))
        logger.info("    " + mkStringColumns(ln map (t => if (t == B) "" else t toPretty), 40 ))
    }
  }
  */

  def formatColumns[A](columns: Seq[Seq[A]], colWidth: Int = 40): List[String] = {
    for (line <- transposeAll(columns, ""))
      yield mkStringColumns(line, colWidth)
  }

  def formatColumnsPretty(columns: Seq[Seq[Term]], colWidth: Int = 40): List[String] =
    formatColumns(columns map (_ map (_.toPretty)), colWidth)

  /**
    * Dumps some things to files, for debug purposes
    */
  def dump(edges: Seq[BaseHyperEdge[Int]], filename: String = "peg")(implicit enc: Encoding) {

    // Dump the encoding
    val encf = new FileWriter("enc")
    val pairs = enc.dumpIdentifiers
    for ((k, v) <- pairs) { encf.write(s"$k $v\n"); }
    encf.close()

    // Dump hyper-edges (PEG)
    def fmtLabel(lab: Int) = enc <-- lab match { case Some(id) => id case None => lab}
    def fmtNode(node: Int) = node
    def fmtNodeLong(node: Int) = enc.dumpTermHint(node)

    val tupf = new FileWriter(filename)
    for (w <- edges sortBy (_.target)) {
      tupf.write(s"${ljust(s"${fmtLabel(w.edgeType)} ${w.drop(1) map fmtNode mkString " "}", 20)}  [${w map fmtNodeLong mkString "] ["}]\n")
    }
    tupf.close()

    /*
    val progf = new FileWriter("prog.json")
    val cc = new DisplayContainer
    val json = cc.map("program" -> state.program,
        "elaborate" -> (state.elaborate map (el => cc.list(List(el.lhs, el.rhs)))))
    progf.write(json.toString)
    progf.close()
    */
  }

}
