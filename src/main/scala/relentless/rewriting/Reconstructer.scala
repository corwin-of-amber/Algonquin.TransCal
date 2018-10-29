package relentless.rewriting

import com.typesafe.scalalogging.LazyLogging
import relentless.matching.Encoding
import relentless.matching.structures.vocabulary.{Trie, Vocabulary}
import syntax.AstSugar._
import syntax.{Identifier, Tree}

import scala.collection.immutable
import scala.collection.immutable.HashSet
import scala.collection.immutable.Stream.cons

/**
  * Reconstruction of terms from tuples stored in the trie.
  *
  * @param init  is the starting term (inner nodes are encoded symbols; leaves are term identifiers)
  * @param words is a stream of tuples
  */
class Reconstructer private(init: Tree[Int], words: Stream[BaseRewriteEdge[Int]]) extends LazyLogging {

  import Reconstructer._

  import collection.mutable

  def this(root: Int, trie: Vocabulary[Int, BaseRewriteEdge[Int]]) = this(new Tree(root), trie.toStream)

  def this(root: Int, words: Seq[BaseRewriteEdge[Int]]) = this(new Tree(root), words.toStream)

  def this(tuple: BaseRewriteEdge[Int], trie: Vocabulary[Int, BaseRewriteEdge[Int]]) = this(Reconstructer.tupleToTree(tuple), trie.toStream)

  def this(tuple: BaseRewriteEdge[Int], words: Seq[BaseRewriteEdge[Int]]) = this(Reconstructer.tupleToTree(tuple), words.toStream)

  /**
    * mapping from a target of the edge to an entry
    *
    * Each entry has at most one next leaf to change (we work leftmost) so each entry can appear once.
    * If an entry finished with a leaf (it is an identifier or known term) then it splits into a new entry and
    * an entry to continue working on the finshed leaf.
    * Using leaves to find first is ok as it is ordered (at least for now from bellmaniac. should have a test for it).
    */
  private val targetToEntries: mutable.Map[Int, immutable.LinearSeq[Entry[Int]]] = {
    // There is one problamatic case where we are initialized from an edge. We need to add the edge to the used edges.
    // As we cannot know what the target was we will use negative numbers. To not confuse patterns we will use -999.
    val first = {
      if (init.subtrees.nonEmpty)
        Entry(init, List(new OriginalEdge[Int](init.root, -999, init.subtrees map(_.root))))
      else
        Entry(init, List.empty)
    }

    val result = mutable.Map[Int, immutable.LinearSeq[Entry[Int]]]().withDefaultValue(immutable.LinearSeq.empty)
    result(first.nextTarget) = first +: result(first.nextTarget)
    result
  }

  private def updateTrees(entry: Entry[Int]): Unit = {
    targetToEntries(entry.nextTarget) = targetToEntries(entry.nextTarget) :+ entry
  }

  // From target to rules
  private val targetToRewrites: mutable.Map[Int, HashSet[BaseRewriteEdge[Int]]] =
    mutable.Map[Int, HashSet[BaseRewriteEdge[Int]]]().withDefaultValue(HashSet.empty)

  // input
  private val edges: Stream[BaseRewriteEdge[Int]] = words

  // each step keep calculating to get a depth calculation
  private val nextStep: mutable.Queue[Entry[Int]] = mutable.Queue.empty

  private def apply(except: Set[Int] = Set.empty): Stream[Tree[Int]] = {
    // TODO: Keep trees in a short format and only concat on demand to save memory or build a trie during run
    // TODO: Maybe keep leaves foreach tree to save runtime
    val identifiers: mutable.Set[Int] = mutable.Set.empty
    val isFinal = (num: Int) => identifiers contains num

    // when updating identifiers might need to finish a leaf that was not considered finished
    def advanceStep(newEdge: BaseRewriteEdge[Int]): Stream[Entry[Int]] = {
      if (!isFinal(newEdge.edgeType)) {
        identifiers += newEdge.edgeType
        val relevantEntries = targetToEntries(newEdge.edgeType).toStream filter (_.nextTarget == newEdge.edgeType)
        val newEntries = relevantEntries flatMap (_.advance(isFinal))
        nextStep ++= newEntries
        for (entry <- newEntries)
          updateTrees(entry)
        // TODO: why am i returning relevant entries as they are surely not final?
        relevantEntries append newEntries
      } else Stream.empty
    }

    def edgeStep(newEdge: BaseRewriteEdge[Int]): Stream[Entry[Int]] = {
      val target = newEdge.target
      targetToRewrites(target) += newEdge
      // replace the leaf with the new edge.
      // then update the trees for all targets if there are new leaves
      val newEntries = for (entry <- targetToEntries(target).toStream) yield entry.extend(newEdge, isFinal)
      for (entryStream <- newEntries;
           entry <- entryStream) yield {
        updateTrees(entry)
        nextStep += entry
        entry
      }
    }

    def developStep(): Stream[Entry[Int]] = {
      // TODO: totally lazy flatten
      if (nextStep.isEmpty) Stream.empty
      else halfLazyFlatten(Stream.continually({
        val entry = nextStep.dequeue
        for (edge <- targetToRewrites(entry.nextTarget).toStream;
             newEntry <- entry.extend(edge, isFinal)) yield {
          nextStep += newEntry
          updateTrees(newEntry)
          newEntry
        }
      }).takeWhile((_) => nextStep.nonEmpty))
    }

    val legalEdges: Stream[BaseRewriteEdge[Int]] = edges filter ((e) => !except.contains(e.edgeType))
    val edgesToTrees: Stream[Entry[Int]] = legalEdges flatMap ((e) => edgeStep(e) append advanceStep(e) append developStep)
    // need to filter out entries that have same index. can do so by validating that index is on last leaf.
    val treeStream = edgesToTrees filter
      ((entry: Entry[Int]) => entry.nextIndex == entry.tree.leaves.length - 1) map
      (_.tree) append cons(init, Stream.empty)
    treeStream filter ((t: Tree[Int]) => t.terminals forall (isFinal(_)))

  }

  def apply(enc: Encoding): Stream[Term] = apply(enc, Set.empty)

  def apply(enc: Encoding, except: Set[Identifier]): Stream[Term] = {
    for (t <- apply(except map (enc -->))) yield decode(t)(enc)
  }

  private def decode(t: Tree[Int])(implicit enc: Encoding): Term = {
    //println(s"decode ${t} ${enc.ntor <-- t.root}")
    enc <-- t.root match {
      case Some(r) =>
        r.kind match {
          case "operator" | "connective" | "quantifier" | "marker" => T(r)(t.subtrees map decode)
          case _ =>
            //if (Formula.QUANTIFIERS contains r.literal.toString) throw new RuntimeException(r.kind)
            T(r) :@ (t.subtrees map decode)
        }
      case None => {
        logger.error(s"Identifier not found in encoding for hyperterm: ${t.root}")
        throw new RuntimeException("Missing identifier in encoding")
      }
    }
  }
}

object Reconstructer {

  private def nodeDepth[T](tree: Tree[T], node: Tree[T]): Int = {
    if (tree eq node) 0
    else tree.subtrees.map(nodeDepth(_, node) + 1).find(_ > -1).getOrElse(-2)
  }

  private def leastMutual[T](tree: Tree[T], nodes: Set[Tree[T]]): Tree[T] = {
    val sub: Option[Tree[T]] = tree.subtrees.find((sub: Tree[T]) => nodes forall sub.hasDescendant)
    if (sub.isDefined) leastMutual(sub.get, nodes) else tree
  }

  // In this case tree can be source because it is a subtree (leaf) so hash makes sens and so does equals
  private case class Entry[T](tree: Tree[T], usedEdges: List[BaseRewriteEdge[T]], nextIndex: Int = 0) extends LazyLogging {

    // TODO: implement or delete
    def edgeEquals(other: Entry[T]): Boolean = true

    def canEqual(a: Any): Boolean = a.isInstanceOf[Entry[T]]

    override def equals(that: Any): Boolean =
      that match {
        case that: Entry[T] => that.canEqual(this) && this.hashCode == that.hashCode &&
          (that.tree equals this.tree) && edgeEquals(that)
        case _ => false
      }

    lazy override val hashCode: Int = treeHash(tree)

    def treeHash(t: Tree[T]): Int = (t.terminals map (_.hashCode)).sum

    private val next: Tree[T] = tree.leaves.drop(nextIndex).head
    val nextTarget: T = next root

    /** We want to create a new entry on the same tree if we got to a final (known term or identifier) leaf.
      * We continue to do so until it isn't possible more.
      * WARNING: calling the function twice on the same entry with intersecting final sets will lead to receiving doubles.
      *
      * @param isFinal - predicate for the final set.
      * @return - Stream of new entries
      */
    def advance(isFinal: T => Boolean): Stream[Entry[T]] = {
      if (isFinal(nextTarget) && tree.leaves.length > (nextIndex + 1)) {
        // leaf depth is enough to know which edges were used on it
        val mutualDepth: Int = nodeDepth(tree, leastMutual(tree, Seq(next, tree.leaves.drop(nextIndex + 1).head).toSet))
        val mutualEdges = usedEdges drop (usedEdges.length - mutualDepth - 1)
        val result = Entry(tree, mutualEdges, nextIndex + 1)
        result #:: result.advance(isFinal)
      }
      else Stream.empty
    }

    def extend(edge: BaseRewriteEdge[T], isFinal: T => Boolean): Stream[Entry[T]] = {
      // Replace Descendants only works because all targets are leaves.
      // Rerunning the tree function each time to make sure eq will work correctly
      if (edge.target != nextTarget) {
        logger.error(s"Error on $edge and $tree. Wrong target.")
        Stream.empty
      } else if (usedEdges contains edge) {
        Stream.empty
      } else {
        val newTree: Tree[T] = this.tree.replaceDescendant((next, edgeToTree(edge)))
        val newEntry: Entry[T] = Entry(newTree, edge :: usedEdges, nextIndex)
        newEntry #:: newEntry.advance(isFinal)
      }
    }
  }

  private def edgeToTree[T](edge: BaseRewriteEdge[T]) = new Tree(edge.edgeType, edge.params map (new Tree(_)) toList)

  private def onlyIf[A](cond: Boolean, op: => Seq[A]) = if (cond) op else Seq.empty

  def whileYield[A](cond: => Boolean)(vals: => A) = takeWhileLeft(cond, Iterator.continually(vals))

  private def tupleToTree[A](tuple: BaseRewriteEdge[A]): Tree[A] = new Tree(tuple(0), tuple drop 2 map (new Tree(_)) toList)

  private def takeWhileLeft[A](cond: => Boolean, it: Iterator[A]): Stream[A] = {
    if (!cond) Stream.empty
    else it.next #:: takeWhileLeft(cond, it)
  }

  // TODO: calculate item item and not stream stream
  private def halfLazyFlatten[A](s: Stream[Stream[A]]): Stream[A] = {
    Stream.empty #:: s
    for (s1 <- s;
         a <- s1) yield a
  }
}
