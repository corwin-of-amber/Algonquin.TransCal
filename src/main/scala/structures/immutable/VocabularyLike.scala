package structures.immutable

import structures.immutable.VocabularyLike.Word
import structures.{Explicit, Item}

import scala.collection.{SetLike, immutable}

/**
  * @author tomer
  * @since 11/15/18
  */
trait VocabularyLike[Letter, +This <: VocabularyLike[Letter, This] with immutable.Set[Word[Letter]]]
  extends SetLike[Word[Letter], This] {

  import VocabularyLike.WordRegex

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
  lazy val letters: Set[Letter] = words.flatten

  /** Replaces a letter with another/
    *
    * @param keep The letter to change to.
    * @param change The letter to change from
    * @return The covabulary with the change.
    */
  def replace(keep: Letter, change: Letter): This

  /* --- IterableLike Impl. --- */

  override def iterator: Iterator[Word[Letter]] = words.iterator
}

object VocabularyLike {
  type Word[Letter] = Seq[Letter]
  type LetterPattern[Letter, Id] = Item[Letter, Id]
  type WordRegex[Letter, Id] = Word[LetterPattern[Letter, Id]]
}