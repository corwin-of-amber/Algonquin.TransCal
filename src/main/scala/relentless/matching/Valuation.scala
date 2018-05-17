package relentless.matching

import relentless.rewriting.BaseHyperEdge

import scala.collection.immutable

class Valuation private(array: Array[Option[HyperTerm]]) extends immutable.IndexedSeq[HyperTerm] {

  def this(n: Int) = this(Array.fill[Option[HyperTerm]](n) {None})

  override def length: Int = array.length

  override def apply(idx: Int): HyperTerm = array(idx) getOrElse (
    throw new RuntimeException(s"No hyper term at $idx")
    )

  def isDefined(idx: Int): Boolean = array(idx) isDefined;

  def isEmpty(idx: Int): Boolean = array(idx) isEmpty;


  /**
    * Matches a word against a pattern. The word has concrete letters, whereas the pattern
    * can have holes (negative integers). The valuation assigns concrete letters to some of
    * the holes, such that if h is a hole placeholder (a negative integer), it is assigned
    * valuation(~h). Unassigned holes have valuation(~h) == 0.
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
