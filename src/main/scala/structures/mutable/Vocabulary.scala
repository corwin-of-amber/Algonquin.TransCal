package structures.mutable

import structures.VocabularyLike

/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter] extends structures.Vocabulary[Letter] with VocabularyLike[Letter, Vocabulary[Letter]] {
}
