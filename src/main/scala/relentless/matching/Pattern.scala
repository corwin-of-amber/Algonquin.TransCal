package relentless.matching

import relentless.rewriting.HyperEdge

import scala.collection.immutable

/**
  * @author user
  * @since 4/2/2018
  * @param transformedPattern The pattern with terms
  */
class Pattern(transformedPattern: IndexedSeq[BaseHyperTerm]) extends immutable.IndexedSeq[BaseHyperTerm] {

  lazy val pattern = transformedPattern map(_ match {
    case Placeholder(v) => -(v+1)
    case HyperTerm(v) => v
  })

  override def length: Int = transformedPattern.length

  override def apply(idx: Int): BaseHyperTerm = transformedPattern.apply(idx)

  def lookup(hyperTerm: Trie[Int, HyperEdge[Int]], valuation: Valuation): Seq[HyperEdge[Int]] = {
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
//
//  /**
//    * @param pattern   is negative when pointing to valuation, otherwise its the real value.
//    * @param valuation it positive when has a real value, othwewise its empty
//    */
//  def apply(pattern: IndexedSeq[Int], valuation: IndexedSeq[Int]): Pattern = Pattern(pattern map (ph => if (ph >= 0) RealHyperTerm(ph) else Placeholder(~ph, valuation)), valuation)
}