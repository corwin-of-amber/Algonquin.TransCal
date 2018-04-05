package relentless.matching

import relentless.rewriting.HyperEdge

import scala.collection.immutable

abstract class HyperTerm {
  type HyperTermId = Int

  def value: Option[HyperTermId]
}

object HyperTerm {
  type HyperTermId = Int
}

case class RealHyperTerm(realValue: HyperTerm.HyperTermId) extends HyperTerm {
  override def value: Option[HyperTermId] = Option(realValue)
}

case class ValuationHyperTerm(index:Int,  valuation: IndexedSeq[Int]) extends HyperTerm {
  override def value: Option[HyperTermId] = {
    val c = valuation(index)
    if (c == 0) {
      Option.empty
    } else {
      Option(c)
    }
  }
}

/**
  * @author user
  * @since 4/2/2018
  * @param transformedPattern The pattern with terms
  */
class Pattern(transformedPattern: IndexedSeq[HyperTerm], valuation: IndexedSeq[Int]) extends immutable.IndexedSeq[HyperTerm] {

  override def length: Int = transformedPattern.length
  override def apply(idx: Int): HyperTerm = transformedPattern.apply(idx)

  import HyperTerm.HyperTermId

  /**
    * Matches a word against a pattern. The word has concrete letters, whereas the pattern
    * can have holes (negative integers). The valuation assigns concrete letters to some of
    * the holes, such that if h is a hole placeholder (a negative integer), it is assigned
    * valuation(~h). Unassigned holes have valuation(~h) == 0.
    *
    * @return a new valuation, possibly with more assignments set, if the word matches;
    *         otherwise None.
    */
  def unify(word: HyperEdge[HyperTermId]): Option[IndexedSeq[HyperTermId]] = {
    if (word.length != transformedPattern.length)
      None
    else {
      def matches(letter: HyperTermId, term: HyperTerm): Option[(Int, HyperTermId)] = {
        term match {
          case RealHyperTerm(placeholder) => if (letter == placeholder) // if we got bad value.
            Some((-1, -1))
          else None
          case ValuationHyperTerm(vidx, _) => {
            term.value match {
              case None => Some((vidx, letter)) // Unassigned holes. positive vidx
              case Some(otherLetter) => if (letter == otherLetter) // if we got bad value.
                Some((-1, -1))
              else None
            }
          }
        }
      }

      val `valuation'`: Array[Int] = valuation.toArray
      for ((letter, term) <- word zip transformedPattern) {
        matches(letter, term) match {
          case None => return None // TODO: Should we throw an error and catch it letter?
          case Some((vidx, newLetter)) => if (vidx >= 0)  //  a new valuation, possibly with more assignments set.
            `valuation'`(vidx) = newLetter
        }
      }
      Some(`valuation'`)
    }
  }

  def lookup(hyperTerm: Trie[HyperTermId, HyperEdge[HyperTermId]]): Seq[HyperEdge[HyperTermId]] = {
    var t = hyperTerm
    try {
      for ((term, idx) <- transformedPattern.zipWithIndex) {
        term.value match {
          case None => return Seq.empty
          case Some(c) => if (c > 0) t = t.get(idx, c) getOrElse {
            return Seq.empty
          }
        }
      }
      t.words
    } catch {
      case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${transformedPattern mkString " "}, valuation = ${valuation mkString " "}; ${e}")
    }
  }
}

object Pattern {
  /**
    * @param pattern is negative when pointing to valuation, otherwise its the real value.
    * @param valuation it positive when has a real value, othwewise its empty
    */
  def apply(pattern: IndexedSeq[Int], valuation: IndexedSeq[Int]): Pattern = Pattern(pattern map(ph=>if (ph>=0) RealHyperTerm(ph) else ValuationHyperTerm(~ph, valuation)), valuation)
}