package structures.immutable

import structures.VocabularyLike

/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter] extends structures.Vocabulary[Letter] with VocabularyLike[Letter, Vocabulary[Letter]]

object Vocabulary {
  def empty[Letter]: Vocabulary[Letter] = Trie.empty

  def apply[Letter](words: Set[Word[Letter]]): Vocabulary[Letter] = Trie(words)

  // Reference structures.Vocabulary
  type Word[Letter] = structures.Vocabulary.Word[Letter]
  type Item[Value, Id] = structures.Vocabulary.Item[Value, Id]
  type Hole[Value, Id] = structures.Vocabulary.Hole[Value, Id]
  val Hole: structures.Vocabulary.Hole.type = structures.Vocabulary.Hole
  type Explicit[Value, Id] = structures.Vocabulary.Explicit[Value, Id]
  val Explicit: structures.Vocabulary.Explicit.type = structures.Vocabulary.Explicit
  type Ignored[Value, Id] = structures.Vocabulary.Ignored[Value, Id]
  val Ignored: structures.Vocabulary.Ignored.type = structures.Vocabulary.Ignored
  type LetterPattern[Letter, Id] = structures.Vocabulary.LetterPattern[Letter, Id]
  type WordPattern[Letter, Id] = structures.Vocabulary.WordPattern[Letter, Id]
}