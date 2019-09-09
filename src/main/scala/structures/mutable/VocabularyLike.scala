package structures.mutable

import structures.Explicit
import structures.VocabularyLike.{Word, WordRegex}

import scala.collection.SetLike

/**
  * @author tomer
  * @since 11/15/18
  */
trait VocabularyLike[Letter, +This <: VocabularyLike[Letter, This] with Set[Word[Letter]]]
  extends SetLike[Word[Letter], This] {

  /** Find words by a regex.
    *
    * @param pattern The prefix to find.
    * @tparam Id A reference type to show repetition connection in the pattern.
    * @return The matching words.
    */
  def findRegex[Id](pattern: WordRegex[Letter, Id]): Set[(Word[Letter], Map[Id, Letter])]
  def findRegexWords[Id](pattern: WordRegex[Letter, Id]): Set[Word[Letter]] = findRegex(pattern).map(_._1)
  override def contains(elem: Word[Letter]): Boolean = findRegex(elem.map(Explicit(_))).nonEmpty

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

  def -=(word: Word[Letter]): This

  def +=(word: Word[Letter]): This

  /* --- IterableLike Impl. --- */

  override def iterator: Iterator[Word[Letter]] = words.iterator
}
