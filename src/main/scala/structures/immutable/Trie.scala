package structures.immutable

import com.typesafe.scalalogging.LazyLogging
import structures.VocabularyLike.Word
import structures._

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
class Trie[Letter] private (subtries: IndexedSeq[Map[Letter, Trie[Letter]]], val words: Set[Word[Letter]])
  extends generic.TrieLike[Letter, Trie[Letter]] with Vocabulary[Letter] with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {

  override def getSubtriesLength: Int = subtries.length

  /** Needs to be overridden in subclasses. */
  override def empty: Trie[Letter] = Trie.empty

  override lazy val letters: Set[Letter] = subtries.flatMap(_.keySet).toSet

  /** Inner constructor that adds words where this Trie is for specific place */
  private def this(wordsFull: Set[Word[Letter]], trieIndex: Int) =
    this({
      val indexes = wordsFull.flatMap(word => word.drop(trieIndex).zipWithIndex.map{case (letter, index) => (index + trieIndex, letter, word)})
      val subtries = indexes.groupBy(_._1).toIndexedSeq.sortBy(_._1).map { case (index: Int, wordsToIndexes: Set[(Int, Letter, Word[Letter])]) =>
        wordsToIndexes.groupBy(_._2).mapValues(wordsToLetters => new Trie(wordsToLetters.map(_._3), index + 1))
      }
      subtries
    }, wordsFull)

  /** Constructors of all words **/
  def this(words: Set[Word[Letter]]=Set.empty[Word[Letter]]) = this(words, 0)

  /* --- Vocabulary Impl. --- */

  override def +(word: Word[Letter]): Trie[Letter] = if (words.contains(word)) this else addRecursive(word, word)

  override def replace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change, 0)

  override def -(word: Word[Letter]): Trie[Letter] = if (!words.contains(word)) this else removeRecursive(word, word)

  /* --- IterableLike Impl. --- */

  override protected[this] def newBuilder: mutable.Builder[Word[Letter], Trie[Letter]] =
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
      val mapSubtriesRemoved = mapSubtries.mapValues(trie =>
        trie.replaceWithIndex(keep, change, mapSubtriesIndex)
      )

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

  override protected def getSubtrie(index: Int, value: Letter): Option[Trie[Letter]] =
    if (subtries.length > index) subtries(index).get(value)
    else None
}

object Trie {
  def empty[Letter]: Trie[Letter] = new Trie()

  def apply[Letter](words: Set[Word[Letter]]): Trie[Letter] = new Trie(words)
}
