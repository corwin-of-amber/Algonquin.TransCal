package relentless.rewriting

import relentless.RewriteRule
import relentless.matching._
import syntax.AstSugar.{$TI, T, Term, Uid}
import syntax.{Identifier, Scheme}

import scala.collection.mutable
import scala.collection.immutable

/** Dont know yet
  *
  * @param shards
  * @param conclusion
  * @param nHoles
  * @param enc
  */
class CompiledRule private (val shards: List[Bundle], val conclusion: Bundle, val nHoles: Int,
                   val origin: RewriteRule)(implicit enc: Encoding) {

  private def this(pattern: Bundle, conclusion: Bundle, origin: RewriteRule)(implicit enc: Encoding) =
    this(pattern.shuffles, conclusion, pattern.minValuationSize, origin)

  def this(origin: RewriteRule)(implicit enc: Encoding) = {
    this(enc.toBundles(origin.src.vars map (T(_)): _*)(origin.src.template.split(RewriteRule.||>) map (_.split(RewriteRule.|||))).bare,
      enc.toBundle(origin.target.vars map (T(_)): _*)(origin.target.template.split(RewriteRule.|||): _*),
      origin)
  }

  //def fresh(wv: Array[Int]) = enc.ntor --> new Uid  // -- more efficient? but definitely harder to debug
  // For debuging this might be better: T((enc <-- wv(0)).asInstanceOf[Identifier], wv.drop(2) map enc.asTerm toList)
  // It i sbetter to just write tests
  private def fresh(wv: immutable.IndexedSeq[Int]): Int = enc.reserveIndex()

  private def sparseTargetLookup(sparsePattern: Seq[(Int, Int)], t: Trie[Int, BaseRewriteEdge[Int]]): Option[Int] =
    t.sparseLookup(sparsePattern).map(_(1))

  // x-0 -> 1  xs-3 -> 4
  private val parameterIndexes: List[Int] = origin.target.vars map (v => 1 + (origin.src.vars indexOf v))

  /** Return all rewrites after filling in all the holes.
    *
    * @param valuation
    * @param trie containing all available rewrites
    * @return
    */
  def conclude(valuation: Valuation, trie: Trie[Int, BaseRewriteEdge[Int]]): List[BaseRewriteEdge[Int]] = {
    assert(valuation.length >= nHoles)

    def valuation_(i: Int): Option[HyperTerm] = {
      if (i < valuation.length && valuation.isDefined(i)) valuation(i)
      else if (i < valuation.length) None
      // --- introduces an existential
      else Some(HyperTerm(enc --> new Identifier("ex?" + i, "variable", new Uid)))
    }

    // 0 1 4 -> -1 -2 -5 / 0 1 4 map Placeholder  | valuation(i)/0/existential
    val newSubterms: mutable.Map[Placeholder, Option[HyperTerm]] = mutable.Map.empty[Placeholder, Option[HyperTerm]] ++
      ((0 :: parameterIndexes) map (i => (Placeholder(i), valuation_(i))))

    // construct new hyperedges by replacing holes (undefined) with valuation elements and fresh hyperterms
    for (pattern <- conclusion.patterns.reverse) yield {
      val updatedPattern: Pattern = new Pattern(pattern map {
        case x: Placeholder => newSubterms.get(x).map(_.getOrElse(x)).getOrElse(x)
        case x => x
      })

      // Try to find target in trie, if no such target exists create a new hyperterm
      val res = updatedPattern(1) match {
        case ph: Placeholder =>
          val sparsePattern = (0, BaseHyperTerm.toInt(updatedPattern(0))) +: (for (i <- 2 until updatedPattern.length) yield (i, BaseHyperTerm.toInt(updatedPattern(i))))
          val wv1: HyperTerm = sparseTargetLookup(sparsePattern, trie).map(HyperTerm) getOrElse HyperTerm(fresh(updatedPattern.map(BaseHyperTerm.toInt)))
          newSubterms += ph -> Some(wv1)
          RewriteEdge(origin, updatedPattern.updated(1, wv1).map(BaseHyperTerm.toInt))
        case _ => RewriteEdge(origin, updatedPattern.map(BaseHyperTerm.toInt))
      }
      assert(res.forall(_ > 0))
      res
    }
  }
}
