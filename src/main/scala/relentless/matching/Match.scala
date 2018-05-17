package relentless.matching

import collection.immutable
import relentless.rewriting.BaseHyperEdge


class Match[HE <: immutable.IndexedSeq[Int]](val trie: Trie[Int, HE])(implicit val enc: Encoding) {

  /**
    * Returns a stream of possible valuations for given pattern tuples.
    */
  def lookupUnify_*(patterns: List[Pattern], valuation: Valuation): Stream[Valuation] = {
    patterns match {
      case Nil => Stream(valuation)
      case head :: tail =>
        for (w <- head.lookup(trie, valuation).toStream;
             `v'` <- valuation.unify(w, head).toStream;
             `v''` <- lookupUnify_*(tail, `v'`)) yield {
          `v''`
        }
    }
  }

  def matchLookupUnify_*(patterns: List[Pattern], first: BaseHyperEdge[Int], valuation: Valuation): Stream[Valuation] = {
    // TODO: why does this one fail on empty patterns?
    valuation.unify(first, patterns.head) match {
      case Some(unifiedValuation) => lookupUnify_*(patterns.tail, unifiedValuation)
      case _ => Stream.empty
    }
  }

}
