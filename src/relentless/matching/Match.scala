package relentless.matching

import syntax.AstSugar.Term



class Match(val trie: Trie[Int])(implicit val enc: Encoding) {
  

    /**
     * Matches a word against a pattern. The word has concrete letters, whereas the pattern
     * can have holes (negative integers). The valuation assigns concrete letters to some of
     * the holes, such that if h is a hole placeholder (a negative integer), it is assigned
     * valuation(~h). Unassigned holes have valuation(~h) == 0.
     * 
     * @return a new valuation, possibly with more assignments set, if the word matches;
     *   otherwise None.
     */
    def unify(word: Array[Int], pattern: Array[Int], valuation: Array[Int]): Option[Array[Int]] =
      if (word.length == pattern.length) {
        def matches(letter: Int, placeholder: Int) = {
          val vidx = -placeholder - 1
          if (placeholder < 0) valuation(vidx) match {
            case 0 => Some((vidx, letter))
            case otherLetter => if (letter == otherLetter) Some((-1,-1)) else None
          }
          else if (letter == placeholder) Some((-1,-1)) else None
        }
        val `valuation'` = valuation.clone()
        for ((letter, placeholder) <- word zip pattern) {
          matches(letter, placeholder) match {
            case None => return None
            case Some((vidx, letter)) => if (vidx >= 0) `valuation'`(vidx) = letter
          }
        }
        return Some(`valuation'`)
      }
      else None
     
    def lookup(pattern: Array[Int], valuation: Array[Int]): Seq[Array[Int]] = {
      var t = trie
      try {
        for ((ph, idx) <- pattern.zipWithIndex) {
          val c = if (ph >= 0) ph else valuation(~ph)
          if (c > 0) t = t.get(idx, c) getOrElse { return Seq.empty }
        }
        return t.words
      }
      catch { case e: RuntimeException => throw new RuntimeException(s"matching pattern = ${pattern mkString " "}, valuation = ${valuation mkString " "}; ${e}") }
    }
    
    /**
     * Returns a stream of possible valuations for given pattern tuples.
     */
    def lookupUnify_*(pattern: List[Array[Int]], valuation: Array[Int]): Stream[Array[Int]] = {
      pattern match {
        case Nil => Stream(valuation)
        case pat :: pats =>
          for (w <- lookup(pat, valuation).toStream;
               `v'` <- unify(w, pat, valuation).toStream;
               `v''` <- lookupUnify_*(pats, `v'`)) yield `v''`
      }
    }
    
    def matchLookupUnify_*(pattern: List[Array[Int]], first: Array[Int], valuation: Array[Int]): Stream[Array[Int]] = {
      unify(first, pattern.head, valuation) match {
        case Some(valuation) => lookupUnify_*(pattern.tail, valuation)
        case _ => Stream.empty
      }
    }
        
}
