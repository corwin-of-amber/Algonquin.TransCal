package relentless.matching.structures.filling

import relentless.matching.structures.vocabulary.Vocabulary

import scala.collection.immutable

/**
  * @author user
  * @since 4/2/2018
  * @param pattern The pattern with terms
  */
class ImplPattern(val pattern: IndexedSeq[BaseHyperTerm]) extends Pattern {

  override def length: Int = pattern.length

  override def apply(idx: Int): BaseHyperTerm = pattern.apply(idx)

  lazy val indexedPlaceholders: IndexedSeq[(Placeholder, Int)] = pattern.zipWithIndex.filter(_._1.isInstanceOf[Placeholder]).
    map(a => (a._1.asInstanceOf[Placeholder], a._2))
  lazy val indexedHyperterms: IndexedSeq[(HyperTerm, Int)] = pattern.zipWithIndex.filter(_._1.isInstanceOf[HyperTerm]).
    map(a => (a._1.asInstanceOf[HyperTerm], a._2))
  lazy val placeholders: IndexedSeq[Placeholder] = indexedPlaceholders.map(_._1)
  lazy val hyperterms: IndexedSeq[HyperTerm] = indexedHyperterms.map(_._1)

  override def lookup[HE <: immutable.IndexedSeq[Int]](trie: Vocabulary[Int, HE], valuation: Valuation): Seq[HE] = {
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


