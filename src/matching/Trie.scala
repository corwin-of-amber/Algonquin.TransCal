package matching

import collection.mutable

import syntax.Tree
import scala.collection.SeqLike


/**
 * It is a mutable data structure that stores words from E* indexed by letter.
 * A directory is used to claim which letter is used to index a word. Some words
 * are indexed by more than one letter.
 */
class Trie[E](val directory: Tree[Trie.DirectoryEntry]) {
 
  import Trie._
  
  private val DEFAULT_CAPACITY = 5
  
  var words: mutable.ListBuffer[Array[E]] = mutable.ListBuffer.empty  /* please don't change 'words' outside this class :-P */
  private var subtries: Array[mutable.Map[E,Trie[E]]] = new Array(DEFAULT_CAPACITY)

  def add(word: Array[E]) {
    for (t <- directory.subtrees) {
      val idx = t.root.letterIndex
      if (idx < word.length) {
        if (subtries(idx) == null) subtries(idx) = mutable.Map.empty
        val subtrie =
          subtries(idx) get word(idx) match {
            case Some(subtrie) => subtrie
            case None => val subtrie = makeSubTrie(t); subtries(idx) += word(idx) -> subtrie; subtrie
          }
        subtrie add word
      }
    }
    words += word
  }
  
  def get(index: Int, letter: E) = {
    val subtrie = subtries(index)
    if (subtrie == null) None
    else subtrie get letter
  }
  
  /* this is so it can be overridden easily */
  def makeSubTrie(subdirectory: Tree[DirectoryEntry]) = new Trie[E](subdirectory)
}


object Trie {
  implicit class DirectoryEntry(val letterIndex: Int)
}