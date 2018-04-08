package relentless.matching

import relentless.rewriting.HyperEdge


class Match(val hyperTerm: Trie[Int, HyperEdge[Int]])(implicit val enc: Encoding) {

  /**
    * Returns a stream of possible valuations for given pattern tuples.
    */
  def lookupUnify_*(patterns: List[Pattern], valuation: Valuation): Stream[Valuation] = {
    patterns match {
      case Nil => Stream(valuation)
      case head :: tail =>
        for (w <- head.lookup(hyperTerm, valuation).toStream;
             `v'` <- valuation.unify(w, head).toStream;
             `v''` <- lookupUnify_*(tail, `v'`)) yield {
          `v''`
        }
    }
  }

  def matchLookupUnify_*(patterns: List[Pattern], first: HyperEdge[Int], valuation: Valuation): Stream[Valuation] = {
    // TODO: why does this one fail on empty patterns?
    valuation.unify(first, patterns.head) match {
      case Some(unifiedValuation) => lookupUnify_*(patterns.tail, unifiedValuation)
      case _ => Stream.empty
    }
  }

}
