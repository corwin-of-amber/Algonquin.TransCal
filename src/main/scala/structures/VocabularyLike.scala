package structures

/**
  * @author tomer
  * @since 11/15/18
  */
trait VocabularyLike[Letter, +This <: VocabularyLike[Letter, This]] {

  import VocabularyLike.{Word, WordPattern}

  def findPatternPrefix[Id](pattern: WordPattern[Letter, Id]): Set[Word[Letter]]

  def findPattern[Id](pattern: WordPattern[Letter, Id]): Set[Word[Letter]] = findPatternPrefix(pattern).filter(_.length == pattern.length)

  def isEmpty: Boolean = words.isEmpty

  def words: Set[Word[Letter]]

  def letters: Set[Letter] = words.flatten

  def replace(keep: Letter, change: Letter): This

  def remove(word: Word[Letter]): This

  def add(word: Word[Letter]): This
}

object VocabularyLike {
  type Word[Letter] = Seq[Letter]
  trait Item[Value, Id]
  case class Hole[Value, Id](id: Id) extends Item[Value, Id]
  case class Explicit[Value, Id](value: Value) extends Item[Value, Id]
  case class Ignored[Value, Id]() extends Item[Value, Id]
  type LetterPattern[Letter, Id] = Item[Letter, Id]
  type WordPattern[Letter, Id] = Word[LetterPattern[Letter, Id]]
}