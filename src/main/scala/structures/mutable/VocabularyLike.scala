package structures.mutable

import structures.VocabularyLike.Word

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
trait VocabularyLike[Letter, +This <: VocabularyLike[Letter, This] with mutable.Set[Word[Letter]]]
  extends structures.VocabularyLike[Letter, This] with mutable.SetLike[Word[Letter], This] {
}
