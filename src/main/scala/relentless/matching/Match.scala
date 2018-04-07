package relentless.matching

import relentless.rewriting.HyperEdge
import relentless.matching.Pattern


class Match(val hyperTerm: Trie[Int, HyperEdge[Int]])(implicit val enc: Encoding) {

  /**
    * Returns a stream of possible valuations for given pattern tuples.
    */
  def lookupUnify_*(patterns: List[IndexedSeq[Int]], valuation: Array[Option[HyperTerm]]): Stream[Array[Int]] = {
    patterns match {
      case Nil => Stream(valuation map ((x: Option[HyperTerm]) => x map (_.value) getOrElse(0)))
      case head :: tail =>
        val pattern = new Pattern(Pattern.toHyperTermBase(head))
        for (w <- pattern.lookup(hyperTerm, valuation).toStream;
             `v'` <- pattern.unify(w, valuation).toStream;
             `v''` <- lookupUnify_*(tail, `v'`)) yield {
          `v''`
        }
    }
  }

  def matchLookupUnify_*(patterns: List[IndexedSeq[Int]], first: HyperEdge[Int], valuation: Array[Option[HyperTerm]]): Stream[Array[Int]] = {
    new Pattern(Pattern.toHyperTermBase(patterns.head)).unify(first, valuation) match {
      case Some(unifiedValuation) => lookupUnify_*(patterns.tail, unifiedValuation)
      case _ => Stream.empty
    }
  }

}
