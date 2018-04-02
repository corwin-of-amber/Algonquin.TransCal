package relentless.matching

import relentless.rewriting.HyperEdge

import scala.collection.immutable

/**
  * @author user
  * @since 4/2/2018
  */
case class Pattern(pattern: IndexedSeq[Int], valuation: Array[Int]) extends immutable.IndexedSeq[Int] {
  override def length: Int = pattern.length

  type TermId = Int

  def translate(placeholder: TermId): TermId = if (placeholder >= 0) placeholder else -placeholder - 1

  /**
    * Matches a word against a pattern. The word has concrete letters, whereas the pattern
    * can have holes (negative integers). The valuation assigns concrete letters to some of
    * the holes, such that if h is a hole placeholder (a negative integer), it is assigned
    * valuation(~h). Unassigned holes have valuation(~h) == 0.
    *
    * @return a new valuation, possibly with more assignments set, if the word matches;
    *         otherwise None.
    */
  def unify(word: HyperEdge[TermId]): Option[Array[TermId]] = {
    if (word.length != pattern.length)
      None
    else {
      def matches(letter: TermId, placeholder: TermId) = {
        if (placeholder < 0) {
          val vidx = translate(placeholder)
          valuation(vidx) match {
            case 0 => Some((vidx, letter))
            case otherLetter => if (letter == otherLetter) Some((-1, -1)) else None
          }
        }
        else if (letter == placeholder) Some((-1, -1))
        else None
      }

      val `valuation'` = valuation.clone()
      for ((letter, placeholder) <- word zip pattern) {
        matches(letter, placeholder) match {
          case None => return None
          case Some((vidx, letter)) => if (vidx >= 0) `valuation'`(vidx) = letter
        }
      }
      Some(`valuation'`)
    }
  }

  def lookup(hyperTerm: Trie[TermId, HyperEdge[TermId]): Seq[HyperEdge[TermId]] = {
    var t = hyperTerm
    try {
      for ((ph, idx) <- pattern.zipWithIndex) {
        val c = if (ph >= 0) ph else valuation(translate(ph))
        if (c > 0) t = t.get(idx, c) getOrElse {
          return Seq.empty
        }
      }
      return t.words
    }
    catch {
      case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${pattern mkString " "}, valuation = ${valuation mkString " "}; ${e}")
    }
  }
}
