package relentless.matching.structures.vocabulary

import syntax.Tree


/**
 * It is a mutable data structure that stores words from Word indexed by letter.
 * A directory is used to claim which letter is used to index a word. Some words
 * are indexed by more than one letter.
 */
trait Vocabulary[Letter, Word <: IndexedSeq[Letter]] {
  def getSubwordsContaining(from: Letter): Seq[Word]

  def getDirectory: Tree[Vocabulary.DirectoryEntry]
  def toStream: Stream[Word]
  def getWords: Seq[Word]
  def subtriesSize: Int
  def firstSubtrie:Map[Letter,Vocabulary[Letter, Word]]

  /** Adds a word to the Vocabulary.
    *
    * @param word The word to add.
    */
  def add(word: Word): Unit

  /** Finds a Vocabulary in index of a letter.
    *
    * @param index The index to look at
    * @param letter The searched letter
    * @return The relevant Vocabulary if exists.
    */
  def get(index: Int, letter: Letter): Option[Vocabulary[Letter, Word]]

  /** Return all words starts with the letter.
    *
    * @param letter The letter to find
    * @return Words starts with the letter
    */
  def getSubwords(letter: Letter): Seq[Word]

  /** Lookup by pattern.
    * comparing each letter in the relevant index and return first word conforming to given pattern.
    *
    * @param sparsePattern index value pairs to find in trie.
    * @return optional word conforming sparse pattern
    */
  def sparseLookup(sparsePattern: Seq[(Int, Letter)]): Option[Word]

  /**
    * uniques() groups words in given trie by values at locations >= index,
    * then declares hyperedge.target to be equivalent for all words in each group.
    * Output is into equiv.
    */
  def uniques(index: Int, repFun: Seq[Letter] => Letter): Map[Letter, Letter]

  def ++=(words: Iterable[Word]): Vocabulary[Letter, Word] = { words foreach add ; this }
}

object Vocabulary {
  implicit class DirectoryEntry(val letterIndex: Int)
  type Directory = Tree[DirectoryEntry]
}
