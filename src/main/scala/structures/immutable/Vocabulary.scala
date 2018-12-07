package structures.immutable

import structures.VocabularyLike

/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter] extends structures.Vocabulary[Letter] with VocabularyLike[Letter, Vocabulary[Letter]]

object Vocabulary {
  def empty[Letter]: Vocabulary[Letter] = Trie.empty
}