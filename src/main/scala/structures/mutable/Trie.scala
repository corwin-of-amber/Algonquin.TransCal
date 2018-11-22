package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.immutable.{Explicit, Item, NotMatter, Reference}

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
class Trie[Letter](private var subtries: IndexedSeq[mutable.Map[Letter, Trie[Letter]]], var words: Set[Seq[Letter]])
  extends structures.Vocabulary[Letter] with LazyLogging {


  /* --- Constructors --- */

  def this() = {
    this(IndexedSeq.empty, Set.empty)
  }


  /* --- Vocabulary Impl. --- */

  override def add(word: Seq[Letter]): Trie[Letter] = if (!words.contains(word)) { addWithIndex(word, 0) } else this

  override def replace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  override def remove(word: Seq[Letter]): Trie[Letter] = if (words.contains(word)) { removeWithIndex(word, 0) } else this

  override def findByPrefix(sparse: Seq[(Int, Letter)]): Set[Seq[Letter]] = {
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

  def findPatternPrefix[Id](pattern: Seq[Item[Letter, Id]]): Set[Seq[Letter]] = {
    recursiveFindPatternPrefix[Id](pattern, Map.empty)
  }

  /* --- Private Methods --- */

  private def addWithIndex(word: Seq[Letter], index: Int): Trie[Letter] = {
    logger.trace("Add word")
    logger.trace("Make subtries larger if needed")
    subtries =  subtries ++ (0 to word.length - index - subtries.length).map(_=>mutable.Map.empty[Letter, Trie[Letter]])

    logger.trace("Add to indexes")
    for (((letter, mapSubtries), mapIndex) <- word.drop(index).zip(subtries).zipWithIndex) {
      mapSubtries.getOrElseUpdate(letter, Trie.empty).addWithIndex(word, index + 1 + mapIndex)
    }

    logger.trace("Add to set")
    words += word
    this
  }

  private def removeWithIndex(word: Seq[Letter], index: Int): Trie[Letter] = {
    logger.trace("Remove to indexes")
    for (((letter, mapSubtries), mapIndex) <- word.drop(index).zip(subtries).zipWithIndex) {
      mapSubtries(letter).removeWithIndex(word, index + 1 + mapIndex)
      if (mapSubtries(letter).isEmpty) {
        mapSubtries.remove(letter)
      }
    }

    logger.trace("Remove from set")
    words -= word
    this
  }

  private def replaceWithIndex(keep: Letter, change: Letter, index: Int): Trie[Letter] = {
    logger.trace("Replace local words")
    words = words.map(word=>word.map(letter=>if (letter==change) keep else letter))

    logger.trace("Replace in subtries")
    for (mapSubtries <- subtries) {
      logger.trace("Execute replace recursively")
      for (subtrie <-mapSubtries.values) {
        subtrie.replaceWithIndex(keep, change, index + 1)
      }

      logger.trace("Merge change trie to keep trie")
      mapSubtries.remove(change) match {
        case Some(subtrie) =>
          if (mapSubtries.contains(keep)) {
            mapSubtries(keep).addAll(subtrie, index)
          } else {
            mapSubtries(keep) = subtrie
          }
        case _ => {}
      }
    }
    this
  }

  private def addAll(otherTrie: Trie[Letter], index: Int): Unit = {
    for(word <- otherTrie.words) {
      addWithIndex(word, index)
    }
  }

  private def recursiveFindPatternPrefix[Id](pattern: Seq[Item[Letter, Id]], placeholdersMap: Map[Id, Letter]): Set[Seq[Letter]] = {
    pattern match {
      case Nil => words
      case item +: more => {
        item match {
          case Explicit(value) => subtries(0)(value).recursiveFindPatternPrefix(more, placeholdersMap)
          case Reference(id) => {
            if (placeholdersMap.contains(id)) {
              val value = placeholdersMap(id)
              subtries(0)(value).recursiveFindPatternPrefix(more, placeholdersMap)
            } else {
              (for((letter, subtrie) <- subtries(0)) yield subtrie.recursiveFindPatternPrefix(more, placeholdersMap updated (id, letter)))
                .flatten.toSet
            }
          }
          case NotMatter() => (for((_, subtrie) <- subtries(0)) yield subtrie.recursiveFindPatternPrefix(more, placeholdersMap))
            .flatten.toSet
        }
      }
    }
  }
}

object Trie {
  def empty[Letter] = new Trie[Letter]()
}
