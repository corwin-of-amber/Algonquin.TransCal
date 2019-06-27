package structures

import structures.VocabularyLike.Word

import scala.collection.generic.Subtractable
import scala.collection.{GenTraversableOnce, IterableLike}

/**
  * @author tomer
  * @since 11/15/18
  */
trait VocabularyLike[Letter, +This <: VocabularyLike[Letter, This]]
  extends IterableLike[Word[Letter], This] with Subtractable[Word[Letter], This] {
  // TODO: extend Set when we understand how

  import VocabularyLike.WordRegex

  /** Find words by a regex.
    *
    * @param pattern The prefix to find.
    * @tparam Id A reference type to show repetition connection in the pattern.
    * @return The matching words.
    */
  def findRegex[Id](pattern: WordRegex[Letter, Id]): Set[(Word[Letter], Map[Id, Letter])]
  def findRegexWords[Id](pattern: WordRegex[Letter, Id]): Set[Word[Letter]] = findRegex(pattern).map(_._1)

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

  /** Adds word.
    *
    * @param word The word to add.
    * @return The vocabulary with the word.
    */
  def +(word:Word[Letter]): This

  /** Adds words.
    *
    * @param words The words to add.
    * @return The vocabulary with the words.
    */
  def ++[Other <: GenTraversableOnce[Seq[Letter]]](words: Other): This = (repr /: words.seq)(_ + _)

  /* --- IterableLike Impl. --- */

  override def iterator: Iterator[Word[Letter]] = words.iterator

  override def seq: TraversableOnce[Word[Letter]] = this
}

object VocabularyLike {
  type Word[Letter] = Seq[Letter]
  type LetterPattern[Letter, Id] = Item[Letter, Id]
  type WordRegex[Letter, Id] = Word[LetterPattern[Letter, Id]]
}