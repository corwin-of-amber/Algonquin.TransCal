package relentless.matching

import scala.collection.immutable

/**
  * @author user
  * @since 4/2/2018
  * @param pattern The pattern with terms
  */
class Pattern(val pattern: IndexedSeq[BaseHyperTerm]) extends immutable.IndexedSeq[BaseHyperTerm] {

  override def length: Int = pattern.length

  override def apply(idx: Int): BaseHyperTerm = pattern.apply(idx)

  lazy val indexedPlaceholders: IndexedSeq[(Placeholder, Int)] = pattern.zipWithIndex.filter(_._1.isInstanceOf[Placeholder]).
    map(a => (a._1.asInstanceOf[Placeholder], a._2))
  lazy val indexedHyperterms: IndexedSeq[(HyperTerm, Int)] = pattern.zipWithIndex.filter(_._1.isInstanceOf[HyperTerm]).
    map(a => (a._1.asInstanceOf[HyperTerm], a._2))
  lazy val placeholders: IndexedSeq[Placeholder] = indexedPlaceholders.map(_._1)
  lazy val hyperterms: IndexedSeq[HyperTerm] = indexedHyperterms.map(_._1)

  def lookup[HE <: immutable.IndexedSeq[Int]](trie: Trie[Int, HE], valuation: Valuation): Seq[HE] = {
    var t = trie
    try {
      for ((term, idx) <- pattern.zipWithIndex) {
        // In case match fails we exit the whole lookup function with an empty seq
        term match {
          case Placeholder(v) => if (valuation.isDefined(v)) t = t.get(idx, valuation(v).get.value) getOrElse { return Seq.empty }
          case HyperTerm(v) => t = t.get(idx, v) getOrElse { return Seq.empty }
        }
      }
      t.getWords
    } catch {
      case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${pattern mkString " "}, valuation = ${valuation mkString " "}; ${e}")
    }
  }
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

    patterns2 map (_ map subst) map (new Pattern(_))
  }
}
