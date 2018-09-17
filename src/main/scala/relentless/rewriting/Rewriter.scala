package relentless.rewriting

import com.typesafe.scalalogging.LazyLogging
import relentless.matching._
import syntax.AstSugar._
import syntax.{Identifier, Scheme, Tree}

import scala.collection.immutable


/** Representing the rewrite system
  *
  * The rewriting is done by building a Trie of terms.
  * The full term can be recreated using reconstruct
  */
class Rewriter(init: Seq[BaseRewriteEdge[Int]], rewriteRules: List[RewriteRule], val trie: Trie[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding) extends LazyLogging {
  import collection.mutable

  def this(init: Seq[BaseRewriteEdge[Int]], rewriteRules: List[RewriteRule], directory: Tree[Trie.DirectoryEntry])(implicit enc: Encoding) =
    this(init, rewriteRules, new Trie[Int, BaseRewriteEdge[Int]](directory))

  private val match_ = new Match(trie)(enc)

  private val wq = mutable.Queue.empty[BaseRewriteEdge[Int]] ++ init
  private val ws = mutable.Set.empty[BaseRewriteEdge[Int]]

  private val targets = mutable.Set.empty[Int]
  private def exceeded: Boolean = targets.size >= 1000

  def apply(): Unit = {
    while (wq.nonEmpty && !exceeded) {
      val w = wq.dequeue()
      if (ws add w) {
        work(w)
      }
    }
  }

  def stream(): Stream[Seq[BaseRewriteEdge[Int]]] = {
    var i = 0
    Reconstructer.whileYield(wq.nonEmpty) {
      val w = wq.dequeue()
      if (ws add w) {
        work(w)
      }
      val gm = matches(RuleBasedTactic.Markers.goal.leaf) // perhaps generalize to other markers?
      val gmAdded = gm drop i
      i = gm.length
      gmAdded
    }
  }

  def work(w: BaseRewriteEdge[Int]): Unit = {
    //println((w mkString " ") + "   [" + (w map (enc.ntor <--) mkString "] [") + "]")
    targets.add(w.target)
    trie add w
    logger.trace(s"working on word ${w mkString " "}")
    for (r <- rewriteRules) {
      wq.enqueue(r.process(w, trie): _*)
    }

    //for (g <- goal) processRule(g, w)
  }


  def matches(headSymbol: Identifier): Seq[BaseRewriteEdge[Int]] = {
    trie.realGet(0, enc --> headSymbol)
  }

  def nonMatches(headSymbols: Identifier*): Seq[BaseRewriteEdge[Int]] =
    Rewriter.nonMatches(trie.words, headSymbols: _*)

}


object Rewriter extends LazyLogging {
  // TODO: reorder vars such that existentials are last
  def nonMatches[HE <: BaseHyperEdge[Int]](words: Seq[HE], headSymbols: Identifier*)(implicit enc: Encoding): Seq[HE] = {
    val heads = headSymbols map (enc -->)
    words filterNot (heads contains _ (0))
  }

  /**
    * Used for misc debugging
    */
  object trace {
    def apply[HE <: BaseHyperEdge[Int]](rule: RewriteRule, valuation: Array[Int], conclusion: Iterable[HE]) {
      for (w <- conclusion)
        productions += w.toList -> (rule, valuation)
    }

    val productions: collection.mutable.Map[List[Int], (RewriteRule, Array[Int])] = collection.mutable.Map.empty
  }
}

// Note: need to inherit base class to gain the ability to define a case class
abstract class BaseRewriteEdge[T](edgeType: T, target: T, params: Seq[T]) extends
  BaseHyperEdge[T](edgeType, target, params) {}

case class OriginalEdge[T](override val edgeType: T, override val target: T, override val params: Seq[T]) extends
  BaseRewriteEdge[T](edgeType, target, params) {}
case object OriginalEdge {
    def apply[T](seq: Seq[T]): OriginalEdge[T] = OriginalEdge[T](seq(0), seq(1), seq drop 2)
}

case class RewriteEdge[T](origin: RewriteRule, override val edgeType: T, override val target: T, override val params: Seq[T]) extends
  BaseRewriteEdge[T](edgeType, target, params) {}
case object RewriteEdge {
  def apply[T](rewriteRule: RewriteRule, seq: Seq[T]): RewriteEdge[T] = RewriteEdge[T](rewriteRule, seq(0), seq(1), seq drop 2)
}