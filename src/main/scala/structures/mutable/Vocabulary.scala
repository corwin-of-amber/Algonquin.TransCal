package structures.mutable

import structures.VocabularyLike.Word

import scala.collection.mutable


/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter]
  extends structures.Vocabulary[Letter] with mutable.Set[Word[Letter]]
     with VocabularyLike[Letter, Vocabulary[Letter]]  {
  /** Needs to be overridden in subclasses. */
  override def empty: Vocabulary[Letter] = Vocabulary.empty
}

object Vocabulary {
  def empty[Letter]: Vocabulary[Letter] = apply(Set.empty)

  def apply[Letter](words: Set[Word[Letter]]): Vocabulary[Letter] = Trie(words)
}