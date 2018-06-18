package relentless.rewriting

import com.typesafe.scalalogging.LazyLogging
import relentless.RewriteRule
import relentless.matching._
import syntax.AstSugar._
import syntax.{Identifier, Scheme, Tree}

import scala.collection.immutable


/** Representing the rewrite system
  *
  * The rewriting is done by building a Trie of terms.
  * The full term can be recreated using reconstruct
  */
class Rewriter(init: Seq[BaseRewriteEdge[Int]], compiledRules: List[CompiledRule], val trie: Trie[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding) extends LazyLogging {

  import Rewriter._

  import collection.mutable

  def this(init: Seq[BaseRewriteEdge[Int]], compiledRules: List[CompiledRule], directory: Tree[Trie.DirectoryEntry])(implicit enc: Encoding) =
    this(init, compiledRules, new Trie[Int, BaseRewriteEdge[Int]](directory))

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
    for (r <- compiledRules) {
      processRule(r, w)
    }

    //for (g <- goal) processRule(g, w)
  }

  private def processRule(rule: CompiledRule, w: BaseRewriteEdge[Int]): Unit = {
    for (s <- rule.shards) {
      val patterns: List[Pattern] = s.patterns
      for (valuation <- match_.matchLookupUnify_*(patterns, w, new Valuation(rule.nHoles))) {
        //println(s"valuation = ${valuation mkString " "}")
        val add = rule.conclude(valuation, trie)
        trace(rule, for(i <- (0 until valuation.length).toArray) yield {if (valuation.isDefined(i)) valuation(i).value else 0}, add)
        logger.trace(s"added new words using ${s.patterns.map(_.mkString(" ")) mkString (", ")}. words: ${add map (_ mkString " ") mkString ", "}")
        wq.enqueue(add: _*)
      }
    }
  }

  def matches(headSymbol: Identifier): Seq[BaseRewriteEdge[Int]] = {
    trie.realGet(0, enc --> headSymbol)
  }

  def nonMatches(headSymbols: Identifier*): Seq[BaseRewriteEdge[Int]] =
    Rewriter.nonMatches(trie.words, headSymbols: _*)

}


object Rewriter extends LazyLogging {
  import syntax.Formula
  import syntax.Formula._

  def compileRules(rulesSrc: List[RewriteRule])(implicit enc: Encoding): List[CompiledRule] =
    rulesSrc map ((rule: RewriteRule) => new CompiledRule(rule.src, rule.target))

  def compileRule(ruleSrc: RewriteRule)(implicit enc: Encoding): List[CompiledRule] =
    compileRules(List(ruleSrc))

  def nonMatches[HE <: BaseHyperEdge[Int]](words: Seq[HE], headSymbols: Identifier*)(implicit enc: Encoding): Seq[HE] = {
    val heads = headSymbols map (enc -->)
    words filterNot (heads contains _ (0))
  }

  /**
    * Used for misc debugging
    */
  object trace {
    def apply[HE <: BaseHyperEdge[Int]](rule: CompiledRule, valuation: Array[Int], conclusion: Iterable[HE]) {
      for (w <- conclusion)
        productions += w.toList -> (rule, valuation)
    }

    val productions: collection.mutable.Map[List[Int], (CompiledRule, Array[Int])] = collection.mutable.Map.empty
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

case class RewriteEdge[T](override val edgeType: T, override val target: T, override val params: Seq[T]) extends
  BaseRewriteEdge[T](edgeType, target, params) {}
case object RewriteEdge {
  def apply[T](seq: Seq[T]): RewriteEdge[T] = RewriteEdge[T](seq(0), seq(1), seq drop 2)
}