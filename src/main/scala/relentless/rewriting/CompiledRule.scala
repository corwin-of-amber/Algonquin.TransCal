package relentless.rewriting

import relentless.RewriteRule
import relentless.matching.{Bundle, Encoding, Trie, Valuation}
import syntax.AstSugar.{$TI, T, Term, Uid}
import syntax.{Identifier, Scheme}

import scala.collection.mutable
import scala.collection.immutable

/**  Dont know yet
  *
  * @param shards
  * @param conclusion
  * @param nHoles
  * @param parameterIndexes
  * @param enc
  */
class CompiledRule(val shards: List[Bundle], val conclusion: Bundle,
                   val nHoles: Int, val parameterIndexes: List[Int])(implicit enc: Encoding) {

  def this(pattern: Bundle, conclusion: Bundle, parameterIndexes: List[Int])(implicit enc: Encoding) =
    this(pattern.shuffles, conclusion, pattern.minValuationSize, parameterIndexes)

  def this(pattern: Scheme.Template, conclusion: Scheme.Template)(implicit enc: Encoding) = {
    this(enc.toBundles(pattern.vars map (T(_)): _*)(pattern.template.split(RewriteRule.||>) map (_.split(RewriteRule.|||))).bare,
      enc.toBundle(conclusion.vars map (T(_)): _*)(conclusion.template.split(RewriteRule.|||): _*),
      conclusion.vars map (v => 1 + (pattern.vars indexOf v)))
  }

  //def fresh(wv: Array[Int]) = enc.ntor --> new Uid  // -- more efficient? but definitely harder to debug
  // For debuging this might be better: T((enc <-- wv(0)).asInstanceOf[Identifier], wv.drop(2) map enc.asTerm toList)
  // It i sbetter to just write tests
  private def fresh(wv: immutable.IndexedSeq[Int]): Int = enc.reserveIndex()

  private def lookup(sparsePattern: Seq[(Int, Int)], t: Trie[Int, BaseRewriteEdge[Int]]): Option[Int] =
    t.sparseLookup(sparsePattern).map(_(1))

  def conclude(valuation: Valuation, trie: Trie[Int, BaseRewriteEdge[Int]]): List[BaseRewriteEdge[Int]] = {
    assert(valuation.length >= nHoles)

    def valuation_(i: Int) = {
      if (i < valuation.length && valuation.isDefined(i)) valuation(i).value
      else if (i < valuation.length) 0
      // --- introduces an existential
      // TODO: Add namespace and kind
      else enc --> new Identifier("ex?" + i,"variable", new Uid)
    }

    val newSubterms = mutable.Map.empty[Int, Int] ++ (0 :: parameterIndexes map (i => (~i, valuation_(i))))

    // construct new tuples by replacing holes (negative integers) with valuation elements and fresh terms
    for (w <- conclusion.tuples.reverse) yield {
      val wv = w map { case x if x < 0 => newSubterms.getOrElse(x, x) case x => x }
      if (wv(1) < 0) {
        val wv1 = lookup((0, wv(0)) +: (for (i <- 2 until wv.length) yield (i, wv(i))), trie) getOrElse fresh(wv)
        newSubterms += wv(1) -> wv1
        RewriteEdge(wv.updated(1, wv1))
      } else RewriteEdge(wv)
    }
  }
}
