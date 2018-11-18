package relentless.matching.structures.vocabulary

import com.typesafe.scalalogging.LazyLogging
import relentless.matching.structures.vocabulary.Vocabulary.DirectoryEntry
import syntax.Tree

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * It is a mutable data structure that stores words from Letter* indexed by letter.
 * A directory is used to claim which letter is used to index a word. Some words
 * are indexed by more than one letter.
 */
class Trie[Letter, Word <: IndexedSeq[Letter]](val directory: Tree[Vocabulary.DirectoryEntry]) extends Vocabulary[Letter, Word] with LazyLogging {
  override def getSubwordsContaining(from: Letter): Seq[Word] = {
//    (for (subtrie <- subtries.toSeq if subtrie != null) yield {
//      subtrie.get(from).map(_.words).getOrElse(Seq.empty)
//    }).flatten.distinct
    // TODO: We are not sure if this is equivalent to the last implementation.
    words.filter(_.contains(from))
  }

  override def getDirectory: Tree[Vocabulary.DirectoryEntry] = directory

  private val DEFAULT_CAPACITY = 5
  
  private var words: ListBuffer[Word] = ListBuffer.empty  /* please don't change 'words' outside this class :-P */

  private val subtries: Array[Map[Letter, Vocabulary[Letter, Word]]] = new Array(DEFAULT_CAPACITY)

  override def toStream: Stream[Word] = words.toStream
  override def getWords: Seq[Word] = words
  override def subtriesSize: Int = subtries.length
  override def firstSubtrie:Map[Letter,Vocabulary[Letter, Word]] = subtries(0)

  override def add(word: Word): Unit = {
    for (t <- directory.subtrees) {
      val idx = t.root.letterIndex
      if (idx < word.length) {
        if (subtries(idx) == null) subtries(idx) = Map.empty
        val subtrie =
          subtries(idx) get word(idx) match {
            case Some(extractedSubtrie) => extractedSubtrie
            case None => val subtrie = makeSubTrie(t); subtries(idx) += word(idx) -> subtrie; subtrie
          }
        subtrie add word
      }
    }
    words += word
  }

  override def get(index: Int, letter: Letter): Option[Vocabulary[Letter, Word]] = {
    val subtrie = if (index < subtries.length) subtries(index) else null
    if (subtrie == null) { /** for debugging if (!(directory.subtrees exists (_.root.letterIndex == index))) throw new RuntimeException(s"trie not indexed by position ${index}"); */
                           None }
    else subtrie get letter
  }

  override def getSubwords(letter: Letter): Seq[Word] = {
    get(0, letter) match {
      case Some(t) => t.getWords
      case None => Seq.empty
    }
  }

  /** Lookup by pattern.
    * comparing each letter in the relevant index and return first word conforming to given pattern.
    *
    * @param sparsePattern index value pairs to find in trie.
    * @return optional word conforming sparse pattern
    */
  override def sparseLookup(sparsePattern: Seq[(Int, Letter)]): Stream[Word] = sparsePattern match {
    case Nil => words.toStream
    case (i, v) +: ivs => get(i, v) match {
      case None => Stream.empty
      case Some(t) => t.sparseLookup(ivs)
    }
  }

  /**
    * uniques() groups words in given trie by values at locations >= index,
    * then declares hyperedge.target to be equivalent for all words in each group.
    * Output is into equiv.
    */
  override def uniques(index: Int, repFun: Seq[Letter] => Letter): Map[Letter, Letter] = {
    val equiv = mutable.Map.empty[Letter, Letter]
    if (index >= subtries.length || subtries(index) == null) {
      if (words.lengthCompare(1) > 0) {
        val equals = words map (_(1))
        val rep = repFun(equals)    /* repFun should make sure there are no cycles; e.g. impose a full order on Letter */
        equals foreach (u => if (u != rep) equiv += u -> rep)
      }
    }
    else {
      for ((_, subtrie) <- subtries(index)) equiv ++= subtrie.uniques(index+1, repFun)
    }
    equiv.toMap
  }

  /* this is so it can be overridden easily */
  private def makeSubTrie(subdirectory: Tree[DirectoryEntry]) = new Trie[Letter, Word](subdirectory)
}
