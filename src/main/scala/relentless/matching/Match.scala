package relentless.matching

import relentless.matching.structures.filling.{Pattern, Valuation}
import relentless.matching.structures.vocabulary.Vocabulary
import relentless.rewriting.BaseHyperEdge

import scala.collection.immutable

class Match[HE <: immutable.IndexedSeq[Int]](val trie: Vocabulary[Int, HE])(implicit val enc: Encoding) {

  /**
    * Returns a stream of possible valuations for given pattern tuples.
    */
  def lookupUnify_*(patterns: List[Pattern], valuation: Valuation): Stream[Valuation] = {
    patterns match {
      case Nil => Stream(valuation)
      case head :: tail =>
        for (w <- head.lookup(trie, valuation).toStream;
             v <- valuation.unify(w, head).toStream;
             vv <- lookupUnify_*(tail, v)) yield {
          vv
        }
    }
  }

  /** Recursively unify patterns with words until all holes in valuation\patterns were filled or a miss match on a
    * literal was found in which case return None. Considering Bundle this means that all holes for vars were filled and
    * also all holes for the new subterms (a new hyper term from the conclusion patterns) were filled.
    *
    * @param patterns The patterns from the Bundle source to match with
    * @param first A HyperEdge to match with
    * @param valuation The valuation we are trying to fill
    * @return The found valuations.
    */
  def matchLookupUnify_*(patterns: List[Pattern], first: BaseHyperEdge[Int], valuation: Valuation): Stream[Valuation] = {
    // TODO: why does this one fail on empty patterns?
    valuation.unify(first, patterns.head) match {
      case Some(unifiedValuation) => lookupUnify_*(patterns.tail, unifiedValuation)
      case _ => Stream.empty
    }
  }

}
