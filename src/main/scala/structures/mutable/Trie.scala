package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.VocabularyLike.Word

import scala.collection.{immutable, mutable}

class Trie[Letter] private(subtries: mutable.Buffer[mutable.Map[Letter, Trie[Letter]]], private val mutableWords: mutable.Set[Word[Letter]])
  extends structures.generic.TrieLike[Letter, Trie[Letter]] with Vocabulary[Letter] with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {

  /** Needs to be overridden in subclasses. */
  override def empty: Trie[Letter] = Trie.empty

  override def clone = new Trie[Letter](mutable.Buffer.empty ++= subtries.map(mutable.Map.empty ++= _.mapValues(_.clone)), mutable.Set.empty ++= mutableWords.clone)
  override def letters: Set[Letter] = subtries.flatMap(_.keySet).toSet

  override def getSubtriesLength: Int = subtries.length

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

  override def replace(keep: Letter, change: Letter): Trie[Letter] = clone.replaceInPlace(keep, change)

  override def replaceInPlace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  override def -(word: Word[Letter]): Trie[Letter] = if (!words.contains(word)) this else Trie[Letter](this.words) -= word

  def -=(word: Word[Letter]): this.type = if (!words.contains(word)) this else removeRecursive(word, word)

  /* --- IterableLike Impl. --- */

  override protected[this] def newBuilder: mutable.Builder[Word[Letter], Trie[Letter]] =
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

  override protected def getSubtrie(index: Int, value: Letter): Option[Trie[Letter]] =
    if (subtries.length > index) subtries(index).get(value)
    else None
}

object Trie {
  def empty[Letter]: Trie[Letter] = new Trie()

  def apply[Letter](words: Set[Word[Letter]]): Trie[Letter] = new Trie(words)
}
