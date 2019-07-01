package structures.immutable

import structures.immutable.VocabularyLike.Word

import scala.collection.immutable

/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter] extends immutable.Set[Word[Letter]] with VocabularyLike[Letter, Vocabulary[Letter]] {
  /** Needs to be overridden in subclasses. */
  override def empty: Vocabulary[Letter] = Vocabulary.empty
}

object Vocabulary {
  def empty[Letter]: Vocabulary[Letter] = Trie.empty

  def apply[Letter](words: Set[Word[Letter]]): Vocabulary[Letter] = Trie(words)
}