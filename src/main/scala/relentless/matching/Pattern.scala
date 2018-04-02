package relentless.matching

import relentless.rewriting.HyperEdge

import scala.collection.immutable

/**
  * @author user
  * @since 4/2/2018
  * @param pattern is negative when pointing to valuation, otherwise its the real value.
  * @param valuation it positive when has a real value, othwewise its empty
  */
case class Pattern(pattern: IndexedSeq[Int], valuation: Array[Int]) extends immutable.IndexedSeq[Int] {
  override def length: Int = pattern.length

  type TermId = Int

  /** Also ~pointer - the logic not on integer.
   *
   * @param pointer Points to valuation.
   * @return Translation for the pointer.
   */
  private def translate(pointer: TermId): TermId = -pointer - 1

  /**
   * Taking the value - if needed going to  valuation
    * TODO: Replace this with Option - it more logical to use it here.
   * @param placeholder The placeholder from pattern.
   * @return The value of the placeholder if any. (bigger than 0).
   */
  private def evaluate(placeholder: TermId): TermId = if (placeholder >= 0) placeholder else valuation(translate(-placeholder - 1))

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
      def matches(letter: TermId, placeholder: TermId): Option[(Int, TermId)] = {
        if (placeholder >= 0) { // If its a real value
          if (letter == placeholder) Some((-1, -1)) else None
        } else { // if its a pointer to value
          val vidx = translate(placeholder)
          val otherLetter = valuation(vidx)
          if (otherLetter == 0) { //  Unassigned holes
            Some((vidx, letter)) // positive vidx
          } else {
            if (letter == otherLetter)  // if we got bad value.
              Some((-1, -1)) else None
          }
        }
      }

      val `valuation'`: Array[Int] = valuation.clone()
      for ((letter, placeholder) <- word zip pattern) {
        matches(letter, placeholder) match {
          case None => return None // TODO: Should we throw an error and catch it letter?
          case Some((vidx, newLetter)) => if (vidx >= 0)  //  a new valuation, possibly with more assignments set.
            `valuation'`(vidx) = newLetter
        }
      }
      Some(`valuation'`)
    }
  }

  def lookup(hyperTerm: Trie[TermId, HyperEdge[TermId]]): Seq[HyperEdge[TermId]] = {
    var t = hyperTerm
    try {
      for ((ph, idx) <- pattern.zipWithIndex) {
        val c = evaluate(ph)
        if (c > 0) t = t.get(idx, c) getOrElse {
          return Seq.empty
        }
      }
      return t.words
    } catch {
      case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${pattern mkString " "}, valuation = ${valuation mkString " "}; ${e}")
    }
  }
}
