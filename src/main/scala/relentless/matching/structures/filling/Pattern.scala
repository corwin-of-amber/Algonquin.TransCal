package relentless.matching.structures.filling

import relentless.matching.structures.vocabulary.Vocabulary

import scala.collection.immutable

/**
  * @author user
  * @since 4/2/2018
  * @param pattern The pattern with terms
  */
trait Pattern extends immutable.IndexedSeq[BaseHyperTerm] {

  def placeholders: IndexedSeq[Placeholder]

  def lookup[HE <: immutable.IndexedSeq[Int]](trie: Vocabulary[Int, HE], valuation: Valuation): Seq[HE]
}

object Pattern {
  def toHyperTermBase(x: Int): BaseHyperTerm = if (x>=0) HyperTerm(x) else Placeholder(-x - 1)
  def toHyperTermBase(p: IndexedSeq[Int]): IndexedSeq[BaseHyperTerm] = p map toHyperTermBase

  /**
    * Combines to lists of patterns into a single list, renumbering the placeholders to avoid clash.
    * @param patterns1 a sequence of Patterns
    * @param patterns2 a second sequence of Patterns
    * @param commonPlaceholders placeholders specified by commonPlaceholders are preserved and maintain across
    *                           the joint sequence. Other placeholders from patterns2 are shifted so as to make them
    *                           fresh relative to patterns1.
    * @return the joint sequence
    */
  def combinePatterns(patterns1: Seq[Pattern], patterns2: Seq[Pattern], commonPlaceholders: Set[Placeholder]) = {
    patterns1 ++ shiftPatterns(patterns2, patterns2, commonPlaceholders)
  }

  /**
    *
    * @param patterns1 a sequence of Patterns
    * @param patterns2 a second sequence of Patterns
    * @param commonPlaceholders placeholders specified by commonPlaceholders are preserved and maintain across
    *                           the joint sequence. Other placeholders from patterns2 are shifted so as to make them
    *                           fresh relative to patterns1.
    * @return the patterns in pattern2, where all the placeholders that are not common, are shifted to higher
    *         indexes.
    */
  def shiftPatterns(patterns1: Seq[Pattern], patterns2: Seq[Pattern], commonPlaceholders: Set[Placeholder]) = {

    val max_ph =         (for (p1 <- patterns1; ph1 <- p1.placeholders) yield ph1.value).max
    val additional_ph =  (for (p2 <- patterns2; ph2 <- p2.placeholders
                                                if !(commonPlaceholders contains ph2)) yield ph2).distinct
    val renum_ph =       (for ((ph2, i) <- additional_ph.zipWithIndex) yield (ph2, Placeholder(max_ph + i + 1))).toMap

    def subst(element: BaseHyperTerm) = element match {
      case ph: Placeholder => renum_ph.getOrElse(ph, ph)
      case _ => element
    }

    patterns2 map (_ map subst) map (new ImplPattern(_))
  }
}
