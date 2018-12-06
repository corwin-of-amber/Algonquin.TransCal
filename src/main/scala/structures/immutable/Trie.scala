package structures.immutable

import com.typesafe.scalalogging.LazyLogging
import structures.VocabularyLike
import structures.VocabularyLike._

/**
  * @author tomer
  * @since 11/15/18
  */
class Trie[Letter](private val subtries: IndexedSeq[Map[Letter, Trie[Letter]]], val words: Set[Word[Letter]])
  extends Vocabulary[Letter] with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {


  /* --- Constructors --- */

  def this() = {
    this(IndexedSeq.empty, Set.empty)
  }


  /* --- Vocabulary Impl. --- */

  override def add(word: Word[Letter]): Trie[Letter] = if (!words.contains(word)) {
    addWithIndex(word)
  } else this

  override def replace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  override def remove(word: Word[Letter]): Trie[Letter] = if (words.contains(word)) {
    removeWithIndex(word)
  } else this

  override def findByPrefix(sparse: Seq[(Int, Letter)]): Set[Word[Letter]] = {
    logger.trace("Entered findByPrefix")
    sparse match {
      case Nil =>
        logger.trace("sparse is Nil")
        words
      case (index, letter) +: remined => if (subtries.length < index) {
        logger.trace("Subtrie doesn't exist")
        Set.empty
      } else {
        subtries(index).get(letter) match {
          case None =>
            logger.trace("Subtrie doesn't exist")
            Set.empty
          case Some(subtrie) => subtrie.findByPrefix(remined)
        }
      }
    }
  }

  override def findPatternPrefix[Id](pattern: WordPattern[Letter, Id]): Set[Word[Letter]] = {
    logger.trace("find pattern prefix")
    recursiveFindPatternPrefix[Id](pattern, Map.empty)
  }

  override def toString: String = f"Trie (${words.mkString(", ")})"

  /* --- Private Methods --- */

  private def addWithIndex(word: Word[Letter]): Trie[Letter] = {
    logger.trace("Add word")
    logger.trace("Make subtries larger if needed")
    val expendedSubtries = subtries ++ (0 to word.length - subtries.length).map(_ => Map.empty[Letter, Trie[Letter]])

    logger.trace("Add to indexes")
    val newSubtries = (for (((letter, mapSubtries), mapIndex) <- word.toIndexedSeq.zip(expendedSubtries).zipWithIndex) yield {
      mapSubtries + ((letter, mapSubtries.getOrElse(letter, Trie.empty).addWithIndex(word.drop(1 + mapIndex))))
    }) ++ expendedSubtries.drop(word.size)

    logger.trace("Add to set")
    val newWords = words + word
    new Trie(newSubtries, newWords)
  }

  private def removeWithIndex(word: Word[Letter]): Trie[Letter] = {
    logger.trace("Remove with index")
    logger.debug(f"Trying to remove $word")
    val newSubtries = (for (((letter, mapSubtries), mapIndex) <- word.toIndexedSeq.zip(subtries).zipWithIndex) yield {
      val subtrieRemoved = mapSubtries(letter).removeWithIndex(word.drop(1 + mapIndex))
      if (subtrieRemoved.isEmpty) {
        mapSubtries - letter
      } else {
        mapSubtries + ((letter, subtrieRemoved))
      }
    }) ++ subtries.drop(word.size)

    logger.trace("Remove from set")
    val newWords = words - word
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
    otherTrie.words.foldLeft(this)((trie, word) => trie.addWithIndex(word.drop(index)))
  }

  private def recursiveFindPatternPrefix[Id](pattern: WordPattern[Letter, Id], placeholdersMap: Map[Id, Letter]): Set[Word[Letter]] = {
    pattern match {
      case Nil => words
      case item +: more =>
        item match {
          case Explicit(value: Letter) => subtries(0)(value).recursiveFindPatternPrefix(more, placeholdersMap)
          case Hole(id: Id) =>
            if (placeholdersMap.contains(id)) {
              val value = placeholdersMap(id)
              subtries(0)(value).recursiveFindPatternPrefix(more, placeholdersMap)
            } else {
              (for ((letter, subtrie) <- subtries(0)) yield subtrie.recursiveFindPatternPrefix(more, placeholdersMap updated(id, letter)))
                .flatten.toSet
            }
          case Ignored() => (for ((_, subtrie) <- subtries(0)) yield subtrie.recursiveFindPatternPrefix(more, placeholdersMap))
            .flatten.toSet
        }
    }
  }
}

object Trie {
  def empty[Letter] = new Trie[Letter]()
}
