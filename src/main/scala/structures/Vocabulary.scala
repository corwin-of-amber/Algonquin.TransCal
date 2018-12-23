package structures

/**
  * @author tomer
  * @since 11/15/18
  */
trait Vocabulary[Letter] extends VocabularyLike[Letter, Vocabulary[Letter]]

object Vocabulary {
  // Reference VocabularyLike
  type Word[Letter] = VocabularyLike.Word[Letter]
  type Item[Value, Id] = VocabularyLike.Item[Value, Id]
  type Hole[Value, Id] = VocabularyLike.Hole[Value, Id]
  val Hole: VocabularyLike.Hole.type = VocabularyLike.Hole
  type Explicit[Value, Id] = VocabularyLike.Explicit[Value, Id]
  val Explicit: VocabularyLike.Explicit.type = VocabularyLike.Explicit
  type Ignored[Value, Id] = VocabularyLike.Ignored[Value, Id]
  val Ignored: VocabularyLike.Ignored.type = VocabularyLike.Ignored
  type LetterPattern[Letter, Id] = VocabularyLike.LetterPattern[Letter, Id]
  type WordPattern[Letter, Id] = VocabularyLike.WordPattern[Letter, Id]
}