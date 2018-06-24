package relentless.matching

import relentless.rewriting.BaseHyperEdge

import scala.collection.immutable

class Valuation private(array: Array[Option[HyperTerm]]) extends immutable.IndexedSeq[Option[HyperTerm]] {

  def this(n: Int) = this(Array.fill[Option[HyperTerm]](n) {None})

  override def length: Int = array.length

  override def apply(idx: Int): Option[HyperTerm] = array(idx)

  // TODO: remove unnecessary functions
  def isDefined(idx: Int): Boolean = array(idx) isDefined;

  def isEmpty(idx: Int): Boolean = array(idx) isEmpty;

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
  def unify[HE <: IndexedSeq[Int]](word: HE, pattern: Pattern): Option[Valuation] = {
    if (word.length != pattern.length)
      None
    else {
      val newArray = array.clone()
      for ((letter, term) <- word zip pattern) {
        term match {
          case Placeholder(v) =>
            if (newArray(v).isEmpty) newArray(v) = Some(HyperTerm(letter)) else if (newArray(v).get.value != letter) return None
          case HyperTerm(v) =>
            if (v != letter) return None
        }
      }
      Some(new Valuation(newArray))
    }
  }
}
