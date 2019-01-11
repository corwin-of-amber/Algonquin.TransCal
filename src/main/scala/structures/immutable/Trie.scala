package structures.immutable

import com.typesafe.scalalogging.LazyLogging
import structures._
import structures.VocabularyLike._

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
class Trie[Letter] private (subtries: IndexedSeq[Map[Letter, Trie[Letter]]], val words: Set[Word[Letter]])
  extends Vocabulary[Letter] with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {

  /** Inner constructor that adds words where this Trie is for specific place */
  private def this(wordsFull: Set[Word[Letter]], trieIndex: Int) =
    this({
      val indexes = wordsFull.flatMap(word => word.drop(trieIndex).zipWithIndex.map(t => (t._2 + trieIndex, t._1, word)))
      val subtries = indexes.groupBy(_._1).toIndexedSeq.sortBy(_._1).map(t => {
        val (index, wordsToIndexes) = t
        val entry = wordsToIndexes.groupBy(_._2).map(tt => {
          val (letter, wordsToLetters) = tt
          val words = wordsToLetters.map(_._3)
          (letter, new Trie(words, index + 1))
        })
        entry
      })
      subtries
    }, wordsFull)

  /** Constructors of all words **/
  def this(words: Set[Word[Letter]]=Set.empty[Word[Letter]]) = this(words, 0)

  /* --- Vocabulary Impl. --- */

  override def add(word: Word[Letter]): Trie[Letter] = if (words.contains(word)) this else addRecursive(word, word)

  override def addAll(words: Set[Word[Letter]]): Trie[Letter] = words.foldLeft(this)((vocabulary, word) => vocabulary + word)

  override def replace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  override def remove(word: Word[Letter]): Trie[Letter] = if (!words.contains(word)) this else removeRecursive(word, word)

  override def findPatternPrefix[Id](pattern: WordPattern[Letter, Id]): Set[Word[Letter]] = {
    logger.trace("find pattern prefix")
    if (isEmpty) {
      Set.empty
    } else {
      recursiveFindPatternPrefix[Id](pattern, Map.empty)
    }
  }

  override def toString: String = f"Trie (${words.mkString(", ")})"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[Word[Letter], Trie[Letter]] =
    new mutable.LazyBuilder[Word[Letter], Trie[Letter]] {
      override def result(): Trie[Letter] = {
        new Trie(parts.flatten.toSet)
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
    logger.debug(f"Trying to remove $word")
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
    val newSubtries: IndexedSeq[Map[Letter, Trie[Letter]]] = for (mapSubtries <- subtries) yield {
      logger.trace("Execute replace recursively")
      val mapSubtriesRemoved = for ((letter, subtrie) <- mapSubtries) yield {
        (letter, subtrie.replaceWithIndex(keep, change, index + 1))
      }

      logger.trace("Merge change trie to keep trie")
      mapSubtriesRemoved.get(change) match {
        case Some(subtrieRemoved) =>
          val newKeep = if (mapSubtries.contains(keep)) {
            mapSubtries(keep).addAll(subtrieRemoved, index)
          } else {
            subtrieRemoved
          }
          mapSubtriesRemoved + ((keep, newKeep))
        case None => mapSubtriesRemoved
      }
    }
    new Trie(newSubtries, newWords)
  }

  private def addAll(otherTrie: Trie[Letter], index: Int): Trie[Letter] = {
    logger.trace("addAll")
    otherTrie.words.foldLeft(this)((trie, word) => trie.addRecursive(word.drop(index), word))
  }

  private def recursiveFindPatternPrefix[Id](pattern: WordPattern[Letter, Id], placeholdersMap: Map[Id, Letter]): Set[Word[Letter]] = {
    def specificValue(value: Letter, more: WordPattern[Letter, Id],  placeholdersMap: Map[Id, Letter]): Set[Word[Letter]] =
      subtries.head.get(value).map(_.recursiveFindPatternPrefix(more, placeholdersMap)).getOrElse(Set.empty)
    pattern match {
      case Nil => words
      case item +: more =>
        if (subtries.isEmpty) Set.empty
        else item match {
          case Explicit(value: Letter) => specificValue(value, more, placeholdersMap)
          case Hole(id: Id) =>
            placeholdersMap.get(id)
              .map(specificValue(_, more, placeholdersMap))
              .getOrElse((for ((letter, subtrie) <- subtries.head) yield subtrie.recursiveFindPatternPrefix(more, placeholdersMap updated(id, letter))).flatten.toSet)
          case Ignored() => (for ((_, subtrie) <- subtries.head) yield subtrie.recursiveFindPatternPrefix(more, placeholdersMap)).flatten.toSet
        }
    }
  }
}

object Trie {
  def empty[Letter]: Trie[Letter] = new Trie()

  def apply[Letter](words: Set[Word[Letter]]): Trie[Letter] = new Trie(words)
}
