package relentless.rewriting

//import com.typesafe.scalalogging.slf4j.{LazyLogging, Logger}
//import org.slf4j.LoggerFactory
import relentless.matching.{Encoding, Trie}
import syntax.AstSugar._
import syntax.{Identifier, Tree}

import scala.collection.immutable.HashSet
import scala.collection.immutable.Stream.cons
import scala.collection.{immutable, mutable}

/**
  * Reconstruction of terms from tuples stored in the trie.
  *
  * @param init         is the starting term (inner nodes are encoded symbols; leaves are term identifiers)
  * @param words        is a stream of tuples
  * @param indexMapping associates some term identifiers with existing terms -- these will not
  *                     be traversed, instead the value in the mapping will be taken as is.
  */
class Reconstruct private(init: Tree[Int], words: Stream[Array[Int]], indexMapping: mutable.Map[Int, Term] = mutable.Map.empty) {
  //  class Reconstruct private(init: Tree[Int], words: Stream[Array[Int]], indexMapping: mutable.Map[Int, Term] = mutable.Map.empty) extends LazyLogging {

  import Reconstruct._

  import collection.mutable

  def this(root: Int, trie: Trie[Int]) = this(new Tree(root), trie.words.toStream)

  def this(root: Int, words: Seq[Array[Int]]) = this(new Tree(root), words.toStream)

  def this(tuple: Array[Int], trie: Trie[Int]) = this(Reconstruct.tupleToTree(tuple), trie.words.toStream)

  def this(tuple: Array[Int], words: Seq[Array[Int]]) = this(Reconstruct.tupleToTree(tuple), words.toStream)

  def ++(mapping: Map[Int, Term]): Reconstruct = {
    indexMapping ++= mapping
    this
  }

  /**
    * mapping from a target of the edge to an entry
    *
    * Each entry has at most one next leaf to change (we work leftmost) so each entry can appear once.
    * If an entry finished with a leaf (it is an identifier or known term) then it splits into a new entry and
    * an entry to continue working on the finshed leaf.
    * Using leaves to find first is ok as it is ordered (at least for now from bellmaniac. should have a test for it).
    */
  val targetToEntries: mutable.Map[Int, immutable.LinearSeq[Entry[Int]]] = {
    val first = Entry(init, List.empty)
    val result = mutable.Map[Int, immutable.LinearSeq[Entry[Int]]]().withDefaultValue(immutable.LinearSeq.empty)
    for (entry <- first #:: first.advance(indexMapping.contains)) {
      result(entry.nextTarget) = entry +: result(entry.nextTarget)
    }
    result
  }

  def updateTrees(entry: Entry[Int]): Unit = {
    targetToEntries(entry.nextTarget) = targetToEntries(entry.nextTarget) :+ entry
  }

  // From target to rules
  val targetToRewrites: mutable.Map[Int, HashSet[HyperEdge[Int]]] =
    mutable.Map[Int, HashSet[HyperEdge[Int]]]().withDefaultValue(HashSet.empty)

  // input
  val edges: Stream[HyperEdge[Int]] = words map ((word) => HyperEdge[Int](word))

  // each step keep calculating to get a depth calculation
  val nextStep: mutable.Queue[Entry[Int]] = mutable.Queue.empty

  def apply(except: Set[Int] = Set.empty): Stream[Tree[Int]] = {
    // TODO: Keep trees in a short format and only concat on demand to save memory or build a trie during run
    // TODO: Maybe keep leaves foreach tree to save runtime
    val identifiers: mutable.Set[Int] = mutable.Set.empty
    val isFinal = (num: Int) => (indexMapping contains num) || (identifiers contains num)
    //    println("applying reconstruct")

    // when updating identifiers might need to finish a leaf that was not considered finished
    def advanceStep(newEdge: HyperEdge[Int]): Stream[Entry[Int]] = {
      if (!isFinal(newEdge.edgeType)) {
        identifiers += newEdge.edgeType
        val relevantEntries = targetToEntries(newEdge.edgeType).toStream filter (_.nextTarget == newEdge.edgeType)
        val newEntries = relevantEntries flatMap (_.advance(isFinal))
        nextStep ++= newEntries
        for (entry <- newEntries)
          updateTrees(entry)
        relevantEntries append newEntries
      } else Stream.empty
    }

    def edgeStep(newEdge: HyperEdge[Int]): Stream[Entry[Int]] = {
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

    val legalEdges: Stream[HyperEdge[Int]] = edges filter ((e) => !except.contains(e.edgeType))
    val edgesToTrees: Stream[Entry[Int]] = legalEdges flatMap ((e) => edgeStep(e) append advanceStep(e) append developStep)
    // need to filter out entries that have same index. can do so by validating that index is on last leaf.
    val treeStream = edgesToTrees filter
      ((entry: Entry[Int]) => entry.nextIndex == entry.tree.leaves.length - 1) map
      (_.tree) append cons(init, Stream.empty)
    treeStream filter ((t: Tree[Int]) => t.terminals forall (isFinal(_)))

  }

  def apply(enc: Encoding): Stream[Term] = apply(enc, Set.empty)

  def apply(enc: Encoding, except: Set[Identifier]): Stream[Term] = {
    for (t <- apply(except map (enc.ntor -->))) yield decode(t)(enc)
  }

  def decode(t: Tree[Int])(implicit enc: Encoding): Term = {
    //println(s"decode ${t} ${enc.ntor <-- t.root}")
    enc.ntor <-- t.root match {
      case r: Identifier =>
        r.kind match {
          case "operator" | "connective" | "quantifier" | "marker" => T(r)(t.subtrees map decode)
          case _ =>
            //if (Formula.QUANTIFIERS contains r.literal.toString) throw new RuntimeException(r.kind)
            T(r) :@ (t.subtrees map decode)
        }
      case t: Tree[_] => t.asInstanceOf[Term] // hope there are no other trees
      case _ => T(new Identifier("some error", "marker"))
    }
  }
}

object Reconstruct {

  def nodeDepth[T](tree: Tree[T], node: Tree[T]): Int = {
    if (tree eq node) 0
    else tree.subtrees.map(nodeDepth(_, node) + 1).filter(_ > -1).headOption.getOrElse(-2)
  }

  def leastMutual[T](tree: Tree[T], nodes: Set[Tree[T]]): Tree[T] = {
    val sub: Option[Tree[T]] = tree.subtrees.filter((sub: Tree[T]) => nodes forall (sub hasDescendant)).headOption
    if (sub.isDefined) leastMutual(sub.get, nodes) else tree
  }

  // In this case tree can be source because it is a subtree (leaf) so hash makes sens and so does equals
  case class Entry[T](tree: Tree[T], usedEdges: List[HyperEdge[T]], nextIndex: Int = 0) {

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

    def extend(edge: HyperEdge[T], isFinal: T => Boolean): Stream[Entry[T]] = {
      // Replace Descendants only works because all targets are leaves.
      // Rerunning the tree function each time to make sure eq will work correctly
      if (edge.target != nextTarget) {
        println(s"Error on $edge and $tree. Wrong target.")
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

  def edgeToTree[T](edge: HyperEdge[T]) = new Tree(edge.edgeType, edge.params map (new Tree(_)) toList)

  def onlyIf[A](cond: Boolean, op: => Seq[A]) = if (cond) op else Seq.empty

  def whileYield[A](cond: => Boolean)(vals: => A) = takeWhileLeft(cond, Iterator.continually(vals))

  def tupleToTree(tuple: Array[Int]) = new Tree(tuple(0), tuple drop 2 map (new Tree(_)) toList)

  def takeWhileLeft[A](cond: => Boolean, it: Iterator[A]): Stream[A] = {
    if (!cond) Stream.empty
    else it.next #:: takeWhileLeft(cond, it)
  }

  // TODO: calculate item item and not stream stream
  def halfLazyFlatten[A](s: Stream[Stream[A]]): Stream[A] = {
    Stream.empty #:: s
    for (s1 <- s;
         a <- s1) yield a
  }
}
