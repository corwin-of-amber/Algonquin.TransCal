package relentless.matching

import relentless.rewriting.HyperEdge

import scala.collection.immutable

abstract class HyperTerm {
  type HyperTermId = Int

  abstract def value: Option[HyperTermId]
}
object HyperTerm {
  type HyperTermId = Int
}

case class RealHyperTerm(realVaHyperlue: HyperTerm.HyperTermId) extends HyperTerm {
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
  * @param transformetdPattern The pattern with terms
  */
case class Pattern(transformetdPattern: IndexedSeq[HyperTerm], valuation: IndexedSeq[Int]) extends immutable.IndexedSeq[Int] {

  override def length: Int = transformetdPattern.length

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
    if (word.length != transformetdPattern.length)
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
      for ((letter, term) <- word zip transformetdPattern) {
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
      for ((term, idx) <- transformetdPattern.zipWithIndex) {
        term.value match {
          case None => return Seq.empty
          case Some(c) => if (c > 0) t = t.get(idx, c) getOrElse {
            return Seq.empty
          }
        }
      }
      t.words
    } catch {
      case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${transformetdPattern mkString " "}, valuation = ${valuation mkString " "}; ${e}")
    }
  }
}

case object Pattern {
  /**
    * @param pattern is negative when pointing to valuation, otherwise its the real value.
    * @param valuation it positive when has a real value, othwewise its empty
    */
  def apply(pattern: IndexedSeq[Int], valuation: IndexedSeq[Int]) = Pattern(pattern map(ph=>if (ph>=0) RealHyperTerm(ph) else ValuationHyperTerm(~ph, valuation)), valuation)
}