package structures.immutable

import structures.immutable.VocabularyLike.Word

/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter] extends VocabularyLike[Letter, Vocabulary[Letter]]

object Vocabulary {
  def empty[Letter]: Vocabulary[Letter] = Trie.empty

  def apply[Letter](words: Set[Word[Letter]]): Vocabulary[Letter] = Trie(words)
}