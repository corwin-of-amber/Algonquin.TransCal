package structures

import structures.VocabularyLike.Word
import structures.Vocabulary

import scala.collection.immutable.Set

/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter]
  extends collection.Set[Word[Letter]]
    with VocabularyLike[Letter, Vocabulary[Letter]] {

  /** Needs to be overridden in subclasses. */
  override def empty: Vocabulary[Letter] = Vocabulary.empty
}

object Vocabulary {
  def empty[Letter]: Vocabulary[Letter] = Vocabulary.empty

  def apply[Letter](words: Set[Word[Letter]]): Vocabulary[Letter] = Vocabulary(words)

  case class Match[Letter, Id](word: Word[Letter], map: Map[Id, Letter])
}