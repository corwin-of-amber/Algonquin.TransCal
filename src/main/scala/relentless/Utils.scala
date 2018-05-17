package relentless

import com.typesafe.scalalogging.LazyLogging
import relentless.matching.{Encoding, Trie}
import relentless.rewriting.RuleBasedTactic.Markers
import relentless.rewriting.{BaseRewriteEdge, _}
import syntax.AstSugar._
import semantics.Prelude._

import scala.collection.mutable.ListBuffer

object Utils extends LazyLogging {

  def transposeAll[A](xss: List[List[A]], ph: A): List[List[A]] = {
    val buf = new ListBuffer[List[A]]
    var yss = xss
    while (!yss.forall(_.isEmpty)) {
      buf += (yss map (_.headOption getOrElse ph))
      yss = (yss map { case _::tail => tail case _ => Nil })
    }
    buf.toList
  }

  def mkStringColumns[A](l: List[A], colWidth: Int) =
    l map (_.toString) map (s => s ++ (" " * (colWidth - s.length))) mkString " "

  def showem(matches: Seq[BaseRewriteEdge[Int]], trie: Trie[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding) {
    val except = Markers.all map (_.leaf) toSet;
    for (gm <- matches) {
      logger.info(s"${gm mkString " "}")//  [${gm map (enc.ntor <--) mkString "] ["}]");
      for (ln <- transposeAll(gm.toList drop 2 map (x => new Reconstruct(x, trie)(enc, except).toList), B))
        logger.info("    " + mkStringColumns(ln map (t => if (t == B) "" else t toPretty), 40 ))
    }
  }
}
