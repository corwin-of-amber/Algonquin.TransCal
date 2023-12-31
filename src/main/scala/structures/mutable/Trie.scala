package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.{Explicit, Hole, Ignored, Repetition}
import structures.VocabularyLike.{Word, WordRegex}

import scala.collection.mutable
import scala.collection.immutable

class Trie[Letter] private(subtries: mutable.Buffer[mutable.Map[Letter, Trie[Letter]]], private val mutableWords: mutable.Set[Word[Letter]])
  extends Vocabulary[Letter]
    with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {

  /** Needs to be overridden in subclasses. */
  override def empty: Trie[Letter] = Trie.empty

  override def clone = new Trie[Letter](mutable.Buffer.empty ++= subtries.map(mutable.Map.empty ++= _.mapValues(_.clone)), mutable.Set.empty ++= mutableWords.clone)
  override def letters: Set[Letter] = subtries.flatMap(_.keySet).toSet

  /** Inner constructor that translates mutable to immutable */
  private def this(subtries: Seq[Map[Letter, Trie[Letter]]], wordsFull: immutable.Set[Word[Letter]]) =
    this(mutable.Buffer(subtries.map(m => mutable.Map[Letter, Trie[Letter]](m.toSeq: _*)): _*), mutable.Set.empty[Word[Letter]].++=(wordsFull))

  /** Inner constructor that adds words where this Trie is for specific place */
  private def this(wordsFull: immutable.Set[Word[Letter]], trieIndex: Int) =
    this({
      val indexes = wordsFull.flatMap(word => word.drop(trieIndex).zipWithIndex.map { case (letter, index) => (index + trieIndex, letter, word) })
      val subtries = {
        indexes.groupBy(_._1).toIndexedSeq.sortBy(_._1).map { case (index: Int, wordsToIndexes: Set[(Int, Letter, Word[Letter])]) =>
          wordsToIndexes.groupBy(_._2).mapValues(wordsToLetters => new Trie(wordsToLetters.map(_._3), index + 1))
        }
      }
      subtries}, wordsFull)

  /** Constructors of all words **/
  def this(words: immutable.Set[Word[Letter]] = immutable.Set.empty[Word[Letter]]) = this(words, 0)

  /* --- Vocabulary Impl. --- */

  override def words: Set[Word[Letter]] = mutableWords.toSet

  def +=(word: Word[Letter]): this.type = if (words.contains(word)) this else addRecursive(word, word)

  override def replace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  def replaceNotInPlace(keep: Letter, change: Letter): Trie[Letter] = Trie(this.words).replace(keep, change)

  override def -(word: Word[Letter]): Trie[Letter] = if (!words.contains(word)) this else Trie[Letter](this.words) -= word

  def -=(word: Word[Letter]): this.type = if (!words.contains(word)) this else removeRecursive(word, word)

  override def findRegex[Id](pattern: WordRegex[Letter, Id]): Set[(Word[Letter], Map[Id, Letter])] = {
    logger.trace("find pattern prefix")
    if (isEmpty) {
      Set.empty
    } else {
      recursiveFindRegex[Id](pattern, Map.empty, 0, 0)
    }
  }

  override def toString: String = f"Trie (${mutableWords.mkString(", ")})"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[Word[Letter], Trie[Letter]] =
    new mutable.ListBuffer[Word[Letter]].mapResult {
      parts => {
        new Trie(parts.toSet)
      }
    }

  /* --- Private Methods --- */

  private def addRecursive(word: Word[Letter], originalWord: Word[Letter]): this.type = {
    logger.trace("Add word")
    logger.trace("Make subtries larger if needed")
    subtries ++= (0 to word.length - subtries.length).map(_ => mutable.Map.empty[Letter, Trie[Letter]])

    logger.trace("Add to indexes")
    for (((letter, mapSubtries), mapIndex) <- word.zip(subtries).zipWithIndex) {
      mapSubtries(letter) = mapSubtries.getOrElse(letter, Trie.empty).addRecursive(word.drop(1 + mapIndex), originalWord)
    }

    logger.trace("Add to set")
    mutableWords += originalWord
    this
  }

  private def removeRecursive(word: Word[Letter], originalWord: Word[Letter]): this.type = {
    logger.trace("Remove with index")
    logger.trace(f"Trying to remove $word")
    for (((letter, mapSubtries), mapIndex) <- word.zip(subtries).zipWithIndex) {
      mapSubtries(letter).removeRecursive(word.drop(1 + mapIndex), originalWord)
      if (mapSubtries(letter).isEmpty) {
        mapSubtries - letter
      }
    }

    logger.trace("Remove from set")
    mutableWords -= originalWord
    this
  }

  private def replaceWithIndex(keep: Letter, change: Letter, index: Int): Trie[Letter] = {
    logger.trace("Replace local words")

    for (w <- mutableWords.filter(w => w.contains(change))) {
      mutableWords.remove(w)
      mutableWords.add(w.map(letter => if (letter == change) keep else letter))
    }

    logger.trace("Replace in subtries")
    for ((mapSubtries, localIndex) <- subtries.zipWithIndex) {
      val mapSubtriesIndex = index + localIndex + 1
      logger.trace("Execute replace recursively")
      for ((k, trie) <- mapSubtries) {
        mapSubtries(k) = trie.replaceWithIndex(keep, change, mapSubtriesIndex)
      }

      logger.trace("Merge change trie to keep trie")
      mapSubtries.get(change) match {
        case Some(subtrieRemoved) =>
          mapSubtries(keep) = mapSubtries.getOrElse(keep, Trie.empty).addAll(subtrieRemoved, mapSubtriesIndex)
        case None => None
      }
      mapSubtries.remove(change)
    }
    this
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
          case Explicit(value) =>
            specificValue(value, more, placeholdersMap)
          case Hole(id) =>
            placeholdersMap.get(id)
              .map(specificValue(_, more, placeholdersMap))
              .getOrElse(
                subtries.applyOrElse(skip, (_: Int) => Map.empty[Letter, Trie[Letter]])
                  .flatMap { case (letter: Letter, subtrie: Trie[Letter]) => subtrie.recursiveFindRegex(more, placeholdersMap updated(id, letter), length + 1, 0) }
                  .toSet
              )
          case Ignored() =>
            recursiveFindRegex(more, placeholdersMap, length + 1, skip + 1)
          case Repetition(minR, maxR, repeated) =>
            assert(repeated.take(scala.math.min(maxR, subtries.length - more.length)).forall(!_.isInstanceOf[Repetition[Letter, Id]]))
            val results = (for (newPattern <- (minR to scala.math.min(maxR, subtries.length)).map(i => repeated.take(i) ++ more)) yield {
              recursiveFindRegex(newPattern, placeholdersMap, length, skip)
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
