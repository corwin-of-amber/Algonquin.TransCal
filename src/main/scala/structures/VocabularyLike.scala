package structures

import structures.VocabularyLike.Word

import scala.collection.IterableLike

/**
  * @author tomer
  * @since 11/15/18
  */
trait VocabularyLike[Letter, +This <: VocabularyLike[Letter, This]]
  extends IterableLike[Word[Letter], This] {
  // TODO: extend Set when we understand how

  import VocabularyLike.WordPattern

  /** Find words by a regex.
    *
    * @param pattern The prefix to find.
    * @tparam Id A reference type to show repetition connection in the pattern.
    * @return The matching words.
    */
  def findRegex[Id](pattern: WordPattern[Letter, Id]): Set[Word[Letter]]

  /**
    * @return The words in the vocabulary
    */
  def words: Set[Word[Letter]]

  /**
    * @return The letters in the vocabulary
    */
  def letters: Set[Letter] = words.flatten

  /** Replaces a letter with another/
    *
    * @param keep The letter to change to.
    * @param change The letter to change from
    * @return The covabulary with the change.
    */
  def replace(keep: Letter, change: Letter): This

  /** Removes an word from the vocabulary.
    *
    * @param word The word to remove.
    * @return The new vocabulary without the edge.
    */
  def remove(word: Word[Letter]): This
  def -(word:Word[Letter]): This = remove(word)

  /** Adds word.
    *
    * @param word The word to add.
    * @return The vocabulary with the word.
    */
  def add(word: Word[Letter]): This
  def +(word:Word[Letter]): This = add(word)

  /** Adds words.
    *
    * @param words The words to add.
    * @return The vocabulary with the words.
    */
  def addAll(words: Set[Word[Letter]]): This
  def :+(words:Set[Word[Letter]]): This = addAll(words)

  def ++[Other <: VocabularyLike[Letter, Other]](other: Other): This = this :+ other.words

  /* --- IterableLike Impl. --- */

  override def iterator: Iterator[Word[Letter]] = words.iterator

  override def seq: TraversableOnce[Word[Letter]] = this
}

object VocabularyLike {
  type Word[Letter] = Seq[Letter]
  type LetterPattern[Letter, Id] = Item[Letter, Id]
  type WordPattern[Letter, Id] = Word[LetterPattern[Letter, Id]]
}