package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.VocabularyLike.Word

import scala.collection.{immutable, mutable}

class Trie[Letter] private(subtries: mutable.Buffer[mutable.Map[Letter, Trie[Letter]]], private val mutableWords: mutable.Set[Word[Letter]], maxDepth: Int)
  extends structures.generic.TrieLike[Letter, Trie[Letter]] with Vocabulary[Letter] with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {

  /** Needs to be overridden in subclasses. */
  override def empty: Trie[Letter] = Trie.empty

  override def clone = new Trie[Letter](mutable.Buffer.empty ++= subtries.map(mutable.Map.empty ++= _.mapValues(_.clone)), mutable.Set.empty ++= mutableWords.clone, maxDepth)

  override def letters: Set[Letter] = subtries.flatMap(_.keySet).toSet

  override def getSubtriesLength: Int = subtries.length

  /** Inner constructor that translates mutable to immutable */
  private def this(subtries: Seq[Map[Letter, Trie[Letter]]], wordsFull: immutable.Set[Word[Letter]], maxDepth: Int) =
    this(mutable.Buffer(subtries.map(m => mutable.Map[Letter, Trie[Letter]](m.toSeq: _*)): _*), mutable.Set.empty[Word[Letter]].++=(wordsFull), maxDepth)

  /** Inner constructor that adds words where this Trie is for specific place */
  private def this(wordsFull: immutable.Set[Word[Letter]], trieIndex: Int, maxDepth: Int) =
    this({
      if (maxDepth > 0) {
        val indexes = wordsFull.flatMap(word => word.drop(trieIndex).zipWithIndex.map { case (letter, index) => (index + trieIndex, letter, word) })
        val subtries = {
          indexes.groupBy(_._1).toIndexedSeq.sortBy(_._1).map { case (index: Int, wordsToIndexes: Set[(Int, Letter, Word[Letter])]) =>
            wordsToIndexes.groupBy(_._2).mapValues(wordsToLetters => new Trie(wordsToLetters.map(_._3), index + 1, maxDepth - 1))
          }
        }
        subtries
      } else Seq.empty
    }, wordsFull, maxDepth)

  /** Constructors of all words **/
  def this(words: immutable.Set[Word[Letter]] = immutable.Set.empty[Word[Letter]]) = this(words, 0, Trie.MAX_DEPTH)

  /* --- Vocabulary Impl. --- */

  override def words: Set[Word[Letter]] = mutableWords.toSet

  def +=(word: Word[Letter]): this.type = if (mutableWords.contains(word)) this else addRecursive(word, word)

  override def replace(keep: Letter, change: Letter): Trie[Letter] = clone.replaceInPlace(keep, change)

  override def replaceInPlace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  override def -(word: Word[Letter]): Trie[Letter] = if (!mutableWords.contains(word)) this else this.clone() -= word

  def -=(word: Word[Letter]): this.type = if (!mutableWords.contains(word)) this else removeRecursive(word, word)

  /* --- IterableLike Impl. --- */

  override protected[this] def newBuilder: mutable.Builder[Word[Letter], Trie[Letter]] =
    new mutable.ListBuffer[Word[Letter]].mapResult {
      parts => {
        new Trie(parts.toSet)
      }
    }

  override def size: Int = mutableWords.size

  /* --- Private Methods --- */

  private def addRecursive(word: Word[Letter], originalWord: Word[Letter]): this.type = {
    logger.trace("Add word")
    if (maxDepth > 0) {
      logger.trace("Make subtries larger if needed")
      subtries ++= (0 to word.length - subtries.length).map(_ => mutable.Map.empty[Letter, Trie[Letter]])

      logger.trace("Add to indexes")
      for (((letter, mapSubtries), mapIndex) <- word.zip(subtries).zipWithIndex) {
        mapSubtries(letter) = mapSubtries.getOrElse(letter, Trie.emptyWithDepth[Letter](maxDepth - 1)).addRecursive(word.drop(1 + mapIndex), originalWord)
      }
    }

    logger.trace("Add to set")
    mutableWords += originalWord
    this
  }

  private def removeRecursive(word: Word[Letter], originalWord: Word[Letter]): this.type = {
    logger.trace("Remove with index")
    logger.trace(f"Trying to remove $word")
    if (maxDepth > 0) {
      for (((letter, mapSubtries), mapIndex) <- word.zip(subtries).zipWithIndex) {
        mapSubtries(letter).removeRecursive(word.drop(1 + mapIndex), originalWord)
        if (mapSubtries(letter).isEmpty) {
          mapSubtries -= letter
        }
      }
    }

    logger.trace("Remove from set")
    mutableWords -= originalWord
    this
  }


  private def replaceWithIndex(keep: Letter, change: Letter, index: Int): Trie[Letter] = {
    logger.trace("Replace local words")

    // TODO: mutable words should be a map by first letter to help prevent going through all words
    // TODO: after we know the first letter we can use subtries to find the rest of the relevant words
    // TODO: if we got a better replace it might be a good idea to add depth to the trie.
    for (w <- mutableWords.filter(w => w.contains(change))) {
      mutableWords.remove(w)
      mutableWords.add(w.map(letter => if (letter == change) keep else letter))
    }

    if (maxDepth > 0) {
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
            mapSubtries(keep) = mapSubtries.getOrElse(keep, Trie.emptyWithDepth[Letter](maxDepth - 1)).addAll(subtrieRemoved, mapSubtriesIndex)
          case None => None
        }
        mapSubtries.remove(change)
      }
    }
    this
  }

  private def addAll(otherTrie: Trie[Letter], index: Int): Trie[Letter] = {
    logger.trace("addAll")
    otherTrie.foldLeft(this)((trie, word) => trie.addRecursive(word.drop(index), word))
  }

  override protected def getSubtrie(index: Int, value: Letter): Option[Trie[Letter]] = {
    assert(maxDepth > 0)
    if (subtries.length > index) subtries(index).get(value)
    else None
  }

  override def keysByIndex(index: Int): Set[Letter] = {
    assert(maxDepth > 0)
    if (subtries.length > index) subtries(index).keys.toSet
    else Set.empty
  }

  override def getMaxDepth: Int = maxDepth

  override def contains(elem: Word[Letter]): Boolean = mutableWords.contains(elem)
}

object Trie {
  val MAX_DEPTH = 2

  def empty[Letter]: Trie[Letter] = new Trie()

  def apply[Letter](words: Set[Word[Letter]]): Trie[Letter] = new Trie(words)

  private def emptyWithDepth[Letter](depth: Int): Trie[Letter] = new Trie(immutable.Set.empty[Word[Letter]], 0, depth)
}
