package relentless.matching

import syntax.Tree

import scala.collection.{immutable, mutable}


/**
 * It is a mutable data structure that stores words from E* indexed by letter.
 * A directory is used to claim which letter is used to index a word. Some words
 * are indexed by more than one letter.
 */
class Trie[E, HE <: immutable.IndexedSeq[E]](val directory: Tree[Trie.DirectoryEntry]) {
 
  import Trie._
  
  private val DEFAULT_CAPACITY = 5
  
  var words: mutable.ListBuffer[HE] = mutable.ListBuffer.empty  /* please don't change 'words' outside this class :-P */
  /*private*/ var subtries: Array[mutable.Map[E,Trie[E, HE]]] = new Array(DEFAULT_CAPACITY)

  def add(word: HE): Unit = {
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
  
  def get(index: Int, letter: E): Option[Trie[E, HE]] = {
    val subtrie = subtries(index)
    if (subtrie == null) { /** for debugging **/ if (!(directory.subtrees exists (_.root.letterIndex == index))) throw new RuntimeException(s"trie not indexed by position ${index}");
                           None }
    else subtrie get letter
  }
  
  def addAll(words: Iterable[HE]): Trie[E, HE] = { words foreach add ; this }
  def ++=(words: Iterable[HE]) = addAll(words)

  /* this is so it can be overridden easily */
  def makeSubTrie(subdirectory: Tree[DirectoryEntry]) = new Trie[E, HE](subdirectory)
}


object Trie {
  implicit class DirectoryEntry(val letterIndex: Int)
  type Directory = Tree[DirectoryEntry]
}