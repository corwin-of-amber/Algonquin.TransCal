package relentless.matching

import relentless.rewriting.BaseHyperEdge

import scala.collection.immutable

/**
  * @author user
  * @since 4/2/2018
  * @param transformedPattern The pattern with terms
  */
class Pattern(transformedPattern: IndexedSeq[BaseHyperTerm]) extends immutable.IndexedSeq[BaseHyperTerm] {

  override def length: Int = transformedPattern.length

  override def apply(idx: Int): BaseHyperTerm = transformedPattern.apply(idx)

  def lookup[HE <: BaseHyperEdge[Int]](hyperTerm: Trie[Int, HE], valuation: Valuation): Seq[HE] = {
    var t = hyperTerm
    try {
      for ((term, idx) <- transformedPattern.zipWithIndex) {
        // In case match fails we exit the whole lookup function with an empty seq
        term match {
          case Placeholder(v) => if (valuation.isDefined(v)) t = t.get(idx, valuation(v).value) getOrElse { return Seq.empty }
          case HyperTerm(v) => t = t.get(idx, v) getOrElse { return Seq.empty }
        }
      }
      t.words
    } catch {
      case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${transformedPattern mkString " "}, valuation = ${valuation mkString " "}; ${e}")
    }
  }
}

object Pattern {
  def toHyperTermBase(p: IndexedSeq[Int]): IndexedSeq[BaseHyperTerm] =
    p map ((x: Int) => if (x>=0) HyperTerm(x) else Placeholder(-x - 1))
}