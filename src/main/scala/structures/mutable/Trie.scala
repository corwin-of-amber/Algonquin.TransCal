package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.VocabularyLike.Word

import scala.collection.{immutable, mutable}

class Trie[Letter] private(subtries: mutable.Buffer[mutable.Map[Letter, Option[Trie[Letter]]]], private val mutableWords: mutable.Set[Word[Letter]], private val trieIndex: Int)
  extends structures.generic.TrieLike[Letter, Trie[Letter]] with Vocabulary[Letter] with VocabularyLike[Letter, Trie[Letter]] with LazyLogging {

  assert(mutableWords.forall(w => w.drop(trieIndex).zipWithIndex.forall(li => subtries.length > li._2 && subtries(li._2).contains(li._1))))

  /** Needs to be overridden in subclasses. */
  override def empty: Trie[Letter] = Trie.empty

  override def clone = new Trie[Letter](mutable.Buffer.empty ++= subtries.map(m => mutable.Map.empty ++= m.mapValues(o => o.map(t => t.clone))), mutable.Set.empty ++= mutableWords.clone, trieIndex)
  override def letters: Set[Letter] = subtries.flatMap(_.keySet).toSet

  override def getSubtriesLength: Int = subtries.length

  override def hashCode(): Int = mutableWords.hashCode()
  override def equals(that: Any): Boolean = that match {
    case t: Trie[Letter] => t.mutableWords.equals(mutableWords)
    case _ => false
  }

  /** Inner constructor that translates mutable to immutable */
  private def this(subtries: Seq[Map[Letter, Option[Trie[Letter]]]], wordsFull: immutable.Set[Word[Letter]], trieIndex: Int) =
    this(mutable.Buffer.empty ++= subtries.map(m => mutable.Map[Letter, Option[Trie[Letter]]](m.toSeq: _*)), mutable.Set.empty[Word[Letter]].++=(wordsFull), trieIndex)

  /** Inner constructor that adds words where this Trie is for specific place */
  private def this(wordsFull: immutable.Set[Word[Letter]], trieIndex: Int) =
    this({
      val subtries = mutable.Buffer.empty[mutable.Map[Letter, Option[Trie[Letter]]]] ++= (0 until (if(wordsFull.isEmpty) 0 else wordsFull.map(_.length).max - trieIndex)).map(_ => mutable.Map.empty[Letter, Option[Trie[Letter]]])
      wordsFull.foreach(w =>
        if (w.length > trieIndex) w.drop(trieIndex).zipWithIndex.foreach({case (l, i) => subtries(i)(l) = None})
      )
      subtries}, mutable.Set.empty[Word[Letter]].++=(wordsFull), trieIndex)

  /** Constructors of all words **/
  def this(words: immutable.Set[Word[Letter]] = immutable.Set.empty[Word[Letter]]) = this(words, 0)

  /* --- Vocabulary Impl. --- */

  override def words: Set[Word[Letter]] = mutableWords.toSet

  def +=(word: Word[Letter]): this.type = if (words.contains(word)) this else addRecursive(word, word)

  override def replace(keep: Letter, change: Letter): Trie[Letter] = clone.replaceInPlace(keep, change)

  override def replaceInPlace(keep: Letter, change: Letter): Trie[Letter] = replaceWithIndex(keep, change)

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
    subtries ++= (0 until word.length - subtries.length).map(_ => mutable.Map.empty[Letter, Option[Trie[Letter]]])

    logger.trace("Add to indexes")
    for (((letter, mapSubtries), mapIndex) <- word.zip(subtries).zipWithIndex) {
      if (!mapSubtries.contains(letter))
        mapSubtries(letter) = None
      mapSubtries(letter) = mapSubtries(letter).map(t => t.addRecursive(word.drop(1 + mapIndex), originalWord))
    }

    logger.trace("Add to set")
    mutableWords += originalWord
    this
  }

  private def removeRecursive(word: Word[Letter], originalWord: Word[Letter]): this.type = {
    logger.trace("Remove with index")
    logger.trace(f"Trying to remove $word")
    for (((letter, mapSubtries), mapIndex) <- word.zip(subtries).zipWithIndex) {
      mapSubtries(letter).map(t => t.removeRecursive(word.drop(1 + mapIndex), originalWord))
      if ((mapSubtries(letter).isDefined && mapSubtries(letter).get.isEmpty) || (mapSubtries(letter).isEmpty && !words.exists(w => w.length > mapIndex + trieIndex && w(mapIndex + trieIndex) == letter))) {
        mapSubtries -= letter
      }
    }

    logger.trace("Remove from set")
    mutableWords -= originalWord
    this
  }


  private def replaceWithIndex(keep: Letter, change: Letter): Trie[Letter] = {
    logger.trace("Replace local words")

    for (w <- mutableWords.filter(w => w.contains(change))) {
      mutableWords.remove(w)
      mutableWords.add(w.map(letter => if (letter == change) keep else letter))
    }

    logger.trace("Replace in subtries")
    for ((mapSubtries, localIndex) <- subtries.zipWithIndex) {
      logger.trace("Execute replace recursively")
      for ((k, trie) <- mapSubtries) {
        mapSubtries(k) = trie.map(t => t.replaceWithIndex(keep, change))
      }

      logger.trace("Merge change trie to keep trie")
      if (mapSubtries.contains(change)) {
        if (!mapSubtries.contains(keep)) mapSubtries(keep) = None
        mapSubtries(keep) = mapSubtries(keep).map(t => t.addAll(mapSubtries(change).map(_.words).getOrElse(mutableWords.filter(w => w.length > trieIndex + localIndex + 1 && w(trieIndex + localIndex + 1) == change).map(w => w.map(l => {
          if (l == change) keep
          else l
        }))).toSet, trieIndex + localIndex + 1))
        mapSubtries.remove(change)
      }
    }
    this
  }

  private def addAll(otherTrie: Set[Word[Letter]], index: Int): Trie[Letter] = {
    logger.trace("addAll")
    otherTrie.foldLeft(this)((trie, word) => trie.addRecursive(word.drop(index), word))
  }

  override protected def getSubtrie(index: Int, value: Letter): Option[Trie[Letter]] =
    if (subtries.length > index) subtries(index).get(value).map({
      case None =>
        val newT = new Trie[Letter](mutableWords.filter(w => w.length > index + trieIndex && w(index + trieIndex) == value).toSet, index + trieIndex + 1)
        subtries(index)(value) =  Some(newT)
        newT
      case Some(t) => t
    })
    else None
}

object Trie {
  def empty[Letter]: Trie[Letter] = new Trie()

  def apply[Letter](words: Set[Word[Letter]]): Trie[Letter] = new Trie(words)
}
