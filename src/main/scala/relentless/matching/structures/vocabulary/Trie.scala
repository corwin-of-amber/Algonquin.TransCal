package relentless.matching.structures.vocabulary

import com.typesafe.scalalogging.LazyLogging
import syntax.Tree

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * It is a mutable data structure that stores words from E* indexed by letter.
 * A directory is used to claim which letter is used to index a word. Some words
 * are indexed by more than one letter.
 */
class Trie[E, HE <: IndexedSeq[E]](val directory: Tree[Trie.DirectoryEntry]) extends LazyLogging {
  def getSubwordsContaining(from: E): Seq[HE] = {
//    (for (subtrie <- subtries.toSeq if subtrie != null) yield {
//      subtrie.get(from).map(_.words).getOrElse(Seq.empty)
//    }).flatten.distinct
    // TODO: We are not sure if this is equivalent to the last implementation.
    words.filter(_.contains(from))
  }

  import Trie._

  private val DEFAULT_CAPACITY = 5
  
  private var words: ListBuffer[HE] = ListBuffer.empty  /* please don't change 'words' outside this class :-P */

  private var subtries: Array[Map[E,Trie[E, HE]]] = new Array(DEFAULT_CAPACITY)

  def toStream: Stream[HE] = words.toStream
  def getWords: Seq[HE] = words
  def subtriesSize: Int = subtries.length
  def firstSubtrie:Map[E,Trie[E, HE]] = subtries(0)

  def add(word: HE): Unit = {
    for (t <- directory.subtrees) {
      val idx = t.root.letterIndex
      if (idx < word.length) {
        if (subtries(idx) == null) subtries(idx) = Map.empty
        val subtrie =
          subtries(idx) get word(idx) match {
            case Some(subtrie) => subtrie
            case None => val subtrie = makeSubTrie(t); subtries(idx) += word(idx) -> subtrie; subtrie
          }
        subtrie add word
      }
    }
    words += word
  }
  
  def get(index: Int, letter: E): Option[Trie[E, HE]] = {
    val subtrie = subtries(index)
    if (subtrie == null) { /** for debugging **/ if (!(directory.subtrees exists (_.root.letterIndex == index))) throw new RuntimeException(s"trie not indexed by position ${index}");
                           None }
    else subtrie get letter
  }

  def getSubwords(index: Int, letter: E): Seq[HE] = {
    get(index, letter) match {
      case Some(t) => t.words
      case None => Seq.empty
    }
  }

  /** Lookup by pattern.
    * comparing each letter in the relevant index and return first word conforming to given pattern.
    *
    * @param sparsePattern index value pairs to find in trie.
    * @return optional word conforming sparse pattern
    */
  def sparseLookup(sparsePattern: Seq[(Int, E)]): Option[HE] = sparsePattern match {
    case Nil => Some(words.head)
    case (i, v) +: ivs => get(i, v) match {
      case None => None
      case Some(t) => t.sparseLookup(ivs)
    }
  }

  /**
    * uniques() groups words in given trie by values at locations >= index,
    * then declares hyperedge.target to be equivalent for all words in each group.
    * Output is into equiv.
    */
  def uniques(index: Int, repFun: Seq[E] => E): mutable.Map[E, E] = {
    val equiv = mutable.Map.empty[E, E]
    if (index >= subtries.length || subtries(index) == null) {
      if (words.lengthCompare(1) > 0) {
        val equals = words map (_(1))
        val rep = repFun(equals)    /* repFun should make sure there are no cycles; e.g. impose a full order on E */
        equals foreach ((u) => if (u != rep) equiv += u -> rep)
      }
    }
    else {
      for ((k, subtrie) <- subtries(index)) equiv ++= subtrie.uniques(index+1, repFun)
    }
    equiv
  }

  def addAll(words: Iterable[HE]): Trie[E, HE] = { words foreach add ; this }
  def ++=(words: Iterable[HE]) = addAll(words)

  /* this is so it can be overridden easily */
  private def makeSubTrie(subdirectory: Tree[DirectoryEntry]) = new Trie[E, HE](subdirectory)
}


object Trie {
  implicit class DirectoryEntry(val letterIndex: Int)
  type Directory = Tree[DirectoryEntry]
}