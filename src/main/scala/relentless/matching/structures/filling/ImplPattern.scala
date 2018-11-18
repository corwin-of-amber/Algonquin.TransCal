package relentless.matching.structures.filling

import relentless.matching.structures.vocabulary.Vocabulary

/**
  * @author user
  * @since 4/2/2018
  * @param pattern The pattern with terms
  */
class ImplPattern(val pattern: IndexedSeq[BaseHyperTerm]) extends Pattern {

  override def length: Int = pattern.length

  override def apply(idx: Int): BaseHyperTerm = pattern.apply(idx)

  private lazy val indexedPlaceholders: IndexedSeq[(Placeholder, Int)] = pattern.zipWithIndex.filter(_._1.isInstanceOf[Placeholder]).
    map(a => (a._1.asInstanceOf[Placeholder], a._2))
  override lazy val placeholders: IndexedSeq[Placeholder] = indexedPlaceholders.map(_._1)

  override def lookup[HE <: IndexedSeq[Int]](trie: Vocabulary[Int, HE], valuation: Valuation): Seq[HE] = {
    val sparsePattern = (pattern zipWithIndex) filter {
      case (Placeholder(v), _) if !valuation.isDefined(v) => false
      case _ => true
    } map {
      case (Placeholder(v), idx) => (idx, valuation(v).get.value)
      case (HyperTerm(v), idx) => (idx, v)
    }
    trie.sparseLookup(sparsePattern)
  }
}


