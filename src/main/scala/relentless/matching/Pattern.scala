package relentless.matching

import relentless.matching.Pattern.Valuation
import relentless.rewriting.HyperEdge

import scala.collection.immutable

trait BaseHyperTerm extends Any {}

case class HyperTerm(value: Int) extends AnyVal with BaseHyperTerm {}
case class Placeholder(value: Int) extends AnyVal with BaseHyperTerm {}
case class ValuationVal(value: Int) extends AnyVal with BaseHyperTerm {
  def empty: Boolean = value == 0
}


/**
  * @author user
  * @since 4/2/2018
  * @param transformedPattern The pattern with terms
  */
class Pattern(transformedPattern: IndexedSeq[BaseHyperTerm]) extends immutable.IndexedSeq[Int] {

  lazy val pattern = transformedPattern map(_ match {
    case Placeholder(v) => -(v+1)
    case HyperTerm(v) => v
  })

  override def length: Int = transformedPattern.length

  override def apply(idx: Int): Int = pattern.apply(idx)

  /** depending if the term is a placeholder or a hyperterm
    * placeholder: take the valuation from that index.
    * if it is empty return the index and value for it
    * if it is not empty verify the letter at that index
    * hyperterm: verify the letter at that index
    *
    * @param letter the term from the word being matched
    * @param term the current pattern being matched
    * @param valuation the partial assignment to pattern
    * @return None if there is no match. (None, _) if it is a constant and match. (index, value) if we need to assign
    */
  private def matches(letter: Int, term: BaseHyperTerm, valuation: Valuation): Option[(Option[Int], HyperTerm)] = {
    term match {
      case HyperTerm(value) =>
        if (letter == value) Some((None, HyperTerm(0))) // if we got bad value.
        else None
      case Placeholder(valuationIndex) => valuation(valuationIndex) match {
        case None => Some((Some(valuationIndex), HyperTerm(letter))) // Unassigned holes. positive vidx
        case Some(otherLetter) => if (letter == otherLetter.value) Some((None, HyperTerm(0))) else None
      }
    }
  }

  /**
    * Matches a word against a pattern. The word has concrete letters, whereas the pattern
    * can have holes (negative integers). The valuation assigns concrete letters to some of
    * the holes, such that if h is a hole placeholder (a negative integer), it is assigned
    * valuation(~h). Unassigned holes have valuation(~h) == 0.
    *
    * @return a new valuation, possibly with more assignments set, if the word matches;
    *         otherwise None.
    */
  def unify(word: HyperEdge[Int], valuation: Array[Option[HyperTerm]]): Option[Array[Option[HyperTerm]]] = {
    if (word.length != transformedPattern.length)
      None
    else {
      for ((letter, term) <- word zip transformedPattern) {
        matches(letter, term, valuation) match {
          case None => return None
          case Some((Some(vidx), newLetter)) => valuation(vidx) = Some(newLetter)
          case Some((None, _)) =>
        }
      }
      Some(valuation)
    }
  }

  def lookup(hyperTerm: Trie[Int, HyperEdge[Int]], valuation: Valuation): Seq[HyperEdge[Int]] = {
    var t = hyperTerm
    try {
      val exit = () => { return Seq.empty }
      for ((term, idx) <- transformedPattern.zipWithIndex) {
        // In case match fails we exit the whole lookup function with an empty seq
        term match {
          case Placeholder(v) => {
            if (valuation(v).nonEmpty) t = t.get(idx, valuation(v).get.value) getOrElse exit()
            else exit()
          }
          case HyperTerm(v) => t = t.get(idx, v) getOrElse exit()
        }
      }
      t.words
    } catch {
      case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${transformedPattern mkString " "}, valuation = ${valuation mkString " "}; ${e}")
    }
  }
}

object Pattern {
  type Valuation = Array[Option[HyperTerm]]
  implicit def toValuation(arr: Array[Int]): Valuation =
    arr map ((x: Int) => if (x == 0) None else Some(HyperTerm(x)));
  def toHyperTermBase(p: IndexedSeq[Int]): IndexedSeq[BaseHyperTerm] =
    p map ((x: Int) => if (x>=0) HyperTerm(x) else Placeholder(-x - 1))
//
//  /**
//    * @param pattern   is negative when pointing to valuation, otherwise its the real value.
//    * @param valuation it positive when has a real value, othwewise its empty
//    */
//  def apply(pattern: IndexedSeq[Int], valuation: IndexedSeq[Int]): Pattern = Pattern(pattern map (ph => if (ph >= 0) RealHyperTerm(ph) else Placeholder(~ph, valuation)), valuation)
}