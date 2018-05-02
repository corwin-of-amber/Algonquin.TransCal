package relentless.rewriting

import relentless.matching.{Bundle, Encoding, Trie, Valuation}
import syntax.AstSugar.{$TI, T, Term}
import syntax.{Identifier, Scheme}

import scala.collection.mutable

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
    this(enc.toBundles(pattern.vars map (T(_)): _*)(pattern.template.split(Rewriter.||>) map (_.split(Rewriter.|||))).bare,
      enc.toBundle(conclusion.vars map (T(_)): _*)(conclusion.template.split(Rewriter.|||): _*),
      conclusion.vars map (v => 1 + (pattern.vars indexOf v)))
    import Rewriter.RuleOps
    of(pattern.template =:> conclusion.template)
  }

  //def fresh(wv: Array[Int]) = enc.ntor --> new Uid  // -- more efficient? but definitely harder to debug
  private def fresh(wv: Array[Int]): Int =
    enc.ntor --> T((enc.ntor <-- wv(0)).asInstanceOf[Identifier], wv.drop(2) map enc.asTerm toList)

  private def lookup(sparsePattern: Seq[(Int, Int)], t: Trie[Int, BaseRewriteEdge[Int]]): Option[Int] =
    t.sparseLookup(sparsePattern).map(_(1))

  def conclude(valuation: Valuation, trie: Trie[Int, BaseRewriteEdge[Int]]): List[BaseRewriteEdge[Int]] = {
    assert(valuation.length >= nHoles)

    def valuation_(i: Int) = {
      if (i < valuation.length && valuation.isDefined(i)) valuation(i).value
      else if (i < valuation.length) 0
      else enc.ntor --> $TI("?" + i) // --- introduces an existential
    }

    val newSubterms = mutable.Map.empty[Int, Int] ++ (0 :: parameterIndexes map (i => (~i, valuation_(i))))

    //if (dbg != null) println(s"[rewrite] conclude from ${dbg.toPretty}  with  ${newSubterms mapValues (enc.ntor <--)}");

    // construct new tuples by replacing holes (negative integers) with valuation elements and fresh terms
    for (w <- conclusion.tuples.reverse) yield {
      val wv = w map { case x if x < 0 => newSubterms.getOrElse(x, x) case x => x }
      if (wv(1) < 0) {
        val wv1 = lookup((0, wv(0)) +: (for (i <- 2 until wv.length) yield (i, wv(i))), trie) getOrElse fresh(wv)
        newSubterms += wv(1) -> wv1
        wv(1) = wv1
      }
      RewriteEdge(wv)
    }
  }

  /* For debugging and error report */
  private var dbg: Term = null;

  private def of(t: Term) = {
    dbg = t;
    this
  }

}
