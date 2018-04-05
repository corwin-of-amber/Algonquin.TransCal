package relentless.matching

import relentless.rewriting.HyperEdge

class Match(val hyperTerm: Trie[Int, HyperEdge[Int]])(implicit val enc: Encoding) {

  /**
    * Returns a stream of possible valuations for given pattern tuples.
    */
  def lookupUnify_*(patterns: List[IndexedSeq[Int]], valuation: IndexedSeq[Int]): Stream[IndexedSeq[Int]] = {
    patterns match {
      case Nil => Stream(valuation)
      case head :: tail =>
        val pattern = Pattern(head, valuation)
        for (w <- pattern.lookup(hyperTerm).toStream;
             `v'` <- pattern.unify(w).toStream;
             `v''` <- lookupUnify_*(tail, `v'`)) yield {
          `v''`
        }
    }
  }

  def matchLookupUnify_*(patterns: List[IndexedSeq[Int]], first: HyperEdge[Int], valuation: IndexedSeq[Int]): Stream[IndexedSeq[Int]] = {
    Pattern(patterns.head, valuation).unify(first) match {
      case Some(unifiedValuation) => lookupUnify_*(patterns.tail, unifiedValuation)
      case _ => Stream.empty
    }
  }

}
