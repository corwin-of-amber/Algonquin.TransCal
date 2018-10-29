package relentless.matching.structures.vocabulary

import syntax.Tree

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable


/**
 * It is a mutable data structure that stores words from E* indexed by letter.
 * A directory is used to claim which letter is used to index a word. Some words
 * are indexed by more than one letter.
 */
trait Vocabulary[E, HE <: IndexedSeq[E]] {
  def getSubwordsContaining(from: E): Seq[HE]

  def getDirectory: Tree[Vocabulary.DirectoryEntry]
  def toStream: Stream[HE]
  def getWords: Seq[HE]
  def subtriesSize: Int
  def firstSubtrie:Map[E,Vocabulary[E, HE]]

  def add(word: HE): Unit

  def get(index: Int, letter: E): Option[Vocabulary[E, HE]]

  def getSubwords(index: Int, letter: E): Seq[HE]

  /** Lookup by pattern.
    * comparing each letter in the relevant index and return first word conforming to given pattern.
    *
    * @param sparsePattern index value pairs to find in trie.
    * @return optional word conforming sparse pattern
    */
  def sparseLookup(sparsePattern: Seq[(Int, E)]): Option[HE]

  /**
    * uniques() groups words in given trie by values at locations >= index,
    * then declares hyperedge.target to be equivalent for all words in each group.
    * Output is into equiv.
    */
  def uniques(index: Int, repFun: Seq[E] => E): mutable.Map[E, E]

  def addAll(words: Iterable[HE]): Vocabulary[E, HE] = { words foreach add ; this }
  def ++=(words: Iterable[HE]) = addAll(words)
}

object Vocabulary {
  implicit class DirectoryEntry(val letterIndex: Int)
  type Directory = Tree[DirectoryEntry]
}
