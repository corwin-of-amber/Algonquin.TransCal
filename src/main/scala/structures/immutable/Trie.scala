package structures.immutable

import com.typesafe.scalalogging.LazyLogging
import structures._
import structures.immutable.VocabularyLike.{Word, WordRegex}

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
class Trie[Letter] private (subtries: IndexedSeq[Map[Letter, Trie[Letter]]], val words: Set[Word[Letter]])
  extends Vocabulary[Letter] with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {

  /** Needs to be overridden in subclasses. */
  override def empty: Trie[Letter] = Trie.empty

  /** Inner constructor that adds words where this Trie is for specific place */
  private def this(wordsFull: Set[Word[Letter]], trieIndex: Int) =
    this({
      val indexes = wordsFull.flatMap(word => word.drop(trieIndex).zipWithIndex.map(((letter: Letter, index: Int) => (index + trieIndex, letter, word)).tupled))
      val subtries = indexes.groupBy(_._1).toIndexedSeq.sortBy(_._1).map(((index: Int, wordsToIndexes: Set[(Int, Letter, Word[Letter])]) => {
        wordsToIndexes.groupBy(_._2).map(((letter: Letter, wordsToLetters: Set[(Int, Letter, VocabularyLike.Word[Letter])]) => {
          val words = wordsToLetters.map(_._3)
          (letter, new Trie(words, index + 1))
        }).tupled)
      }).tupled)
      subtries
    }, wordsFull)

  /** Constructors of all words **/
  def this(words: Set[Word[Letter]]=Set.empty[Word[Letter]]) = this(words, 0)

  /* --- Vocabulary Impl. --- */

  override def +(word: Word[Letter]): Trie[Letter] = if (words.contains(word)) this else addRecursive(word, word)

  override def replace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  override def -(word: Word[Letter]): Trie[Letter] = if (!words.contains(word)) this else removeRecursive(word, word)

  override def findRegex[Id](pattern: WordRegex[Letter, Id]): Set[(Word[Letter], Map[Id, Letter])] = {
    logger.trace("find pattern prefix")
    if (isEmpty) {
      Set.empty
    } else {
      recursiveFindRegex[Id](pattern, Map.empty, 0, 0)
    }
  }

  override def toString: String = f"Trie (${words.mkString(", ")})"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[Word[Letter], Trie[Letter]] =
    new mutable.ListBuffer[Word[Letter]].mapResult {
      parts => {
        new Trie(parts.toSet)
      }
    }

  /* --- Private Methods --- */

  private def addRecursive(word: Word[Letter], originalWord: Word[Letter]): Trie[Letter] = {
    logger.trace("Add word")
    logger.trace("Make subtries larger if needed")
    val expendedSubtries = subtries ++ (0 to word.length - subtries.length).map(_ => Map.empty[Letter, Trie[Letter]])

    logger.trace("Add to indexes")
    val newSubtries = (for (((letter, mapSubtries), mapIndex) <- word.toIndexedSeq.zip(expendedSubtries).zipWithIndex) yield {
      mapSubtries + ((letter, mapSubtries.getOrElse(letter, Trie.empty).addRecursive(word.drop(1 + mapIndex), originalWord)))
    }) ++ expendedSubtries.drop(word.size)

    logger.trace("Add to set")
    val newWords = words + originalWord
    new Trie(newSubtries, newWords)
  }

  private def removeRecursive(word: Word[Letter], originalWord: Word[Letter]): Trie[Letter] = {
    logger.trace("Remove with index")
    logger.trace(f"Trying to remove $word")
    val newSubtries = (for (((letter, mapSubtries), mapIndex) <- word.toIndexedSeq.zip(subtries).zipWithIndex) yield {
      val subtrieRemoved = mapSubtries(letter).removeRecursive(word.drop(1 + mapIndex), originalWord)
      if (subtrieRemoved.isEmpty) {
        mapSubtries - letter
      } else {
        mapSubtries + ((letter, subtrieRemoved))
      }
    }) ++ subtries.drop(word.size)

    logger.trace("Remove from set")
    val newWords = words - originalWord
    new Trie(newSubtries, newWords)
  }

  private def replaceWithIndex(keep: Letter, change: Letter, index: Int): Trie[Letter] = {
    logger.trace("Replace local words")
    val newWords = words.map(word => word.map(letter => if (letter == change) keep else letter))

    logger.trace("Replace in subtries")
    val newSubtries = subtries.zipWithIndex.map(mapSubtriesWithIndex => {
      val (mapSubtries, localIndex) = mapSubtriesWithIndex
      val mapSubtriesIndex = index + localIndex + 1
      logger.trace("Execute replace recursively")
      val mapSubtriesRemoved = mapSubtries.map(((letter: Letter, trie: Trie[Letter]) => {
        (letter, trie.replaceWithIndex(keep, change, mapSubtriesIndex))
      }).tupled)

      logger.trace("Merge change trie to keep trie")
      mapSubtriesRemoved.get(change) match {
        case Some(subtrieRemoved) =>
          val newKeep = mapSubtriesRemoved.get(keep) match {
            case Some(x) => x.addAll(subtrieRemoved, mapSubtriesIndex)
            case None => subtrieRemoved
          }
          (mapSubtriesRemoved - change) + ((keep, newKeep))
        case None => mapSubtriesRemoved
      }
    })
    new Trie(newSubtries, newWords)
  }

  private def addAll(otherTrie: Trie[Letter], index: Int): Trie[Letter] = {
    logger.trace("addAll")
    otherTrie.foldLeft(this)((trie, word) => trie.addRecursive(word.drop(index), word))
  }

  private def recursiveFindRegex[Id](pattern: WordRegex[Letter, Id], placeholdersMap: Map[Id, Letter], length: Int, skip: Int): Set[(Word[Letter], Map[Id, Letter])] = {
    def specificValue(value: Letter, more: WordRegex[Letter, Id], placeholdersMap: Map[Id, Letter]): Set[(Word[Letter], Map[Id, Letter])] =
      if (subtries.length > skip)
        subtries(skip).get(value).map(_.recursiveFindRegex(more, placeholdersMap, length + 1, 0)).getOrElse(Set.empty)
      else Set.empty
    pattern match {
      case Nil => words.filter(_.length == length).zip(Stream.continually(placeholdersMap))
      case item +: more =>
        item match {
          case Explicit(value: Letter) =>
            specificValue(value, more, placeholdersMap)
          case Hole(id: Id) =>
            placeholdersMap.get(id)
              .map(specificValue(_, more, placeholdersMap))
              .getOrElse((for ((letter, subtrie) <- if (subtries.length > skip) subtries(skip) else Map.empty)
                yield subtrie.recursiveFindRegex(more, placeholdersMap updated(id, letter), length + 1, 0)).flatten.toSet)
          case Ignored() =>
            recursiveFindRegex(more, placeholdersMap, length + 1, skip + 1)
          case Repetition(minR, maxR, repeated) =>
            assert(repeated.take(math.min(maxR, subtries.length - more.length)).forall(!_.isInstanceOf[Repetition[Letter, Id]]))
            val results = (for (newPattern <- (minR to math.min(maxR, subtries.length)).map(i => repeated.take(i) ++ more)) yield {
              recursiveFindRegex(newPattern, placeholdersMap, length, 0)
            }).flatten.toSet
            results
        }
    }
  }
}

object Trie {
  def empty[Letter]: Trie[Letter] = new Trie()

  def apply[Letter](words: Set[Word[Letter]]): Trie[Letter] = new Trie(words)
}
