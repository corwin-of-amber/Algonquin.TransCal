package relentless.matching.structures.filling

import scala.collection.immutable

trait Valuation extends immutable.IndexedSeq[Option[HyperTerm]] {

  // TODO: remove unnecessary functions
  def isDefined(idx: Int): Boolean

  override def toString(): String = 0 until length map (x => if (isDefined(x)) this(x) else 0) mkString " "

  /**
    * Matches a word against a pattern. The word has concrete letters (int of type HyperTerm), whereas the pattern
    * can have holes (int of type Placeholder). The valuation assigns concrete letters to some of
    * the holes, such that if h is a hole placeholder, it is assigned
    * the value valuation(placeholder.value). Unassigned holes have valuation(placeholder.value) == 0.
    *
    * @return a new valuation, possibly with more assignments set, if the word matches;
    *         otherwise None.
    */
  def unify[HE <: IndexedSeq[Int]](word: HE, pattern: Pattern): Option[Valuation]
}
