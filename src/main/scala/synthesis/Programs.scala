package synthesis

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge
import structures.mutable.{HyperGraphManyWithOrderToOne, VocabularyHyperGraph}
import syntax.Tree

/** Programs contains all the available programs holding them for future optimized rewrites and reconstruction of them.
  * @author tomer
  * @since 11/19/18
  */
class Programs(val hyperGraph: HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm]) extends LazyLogging {


  /* --- Constructors --- */

  def this(tree: Tree[Int]) = this(Programs.destruct(tree))


  /* --- Public --- */

  /** Builds trees from of programs where the hyper term is the base program.
    *
    * @param hyperTerm The hyper term to build.
    * @return All the trees.
    */
  def reconstruct(hyperTerm: HyperTerm): Iterator[Tree[Int]] = {
    logger.trace("Reconstruct programs")
    val targetToEdges = hyperGraph.edges.groupBy(edge => edge.target)

    import scala.collection.AbstractIterator

    /** Iterator which combines sequence of iterators (return all combinations of their results).
      * @param iterators All the iterators to combine.
      * @tparam T The return type.
      */
    class CombineSeq[T](iterators: Seq[Iterator[T]]) extends AbstractIterator[Seq[T]] {


      /* --- AbstractIterator Implementation --- */

      override def hasNext: Boolean = innerIterator.hasNext

      override def next(): Seq[T] = innerIterator.next()


      /* --- Privates --- */

      /** Creates inner iterator of CombinedTwo and flat map it out. */
      private val innerIterator = iterators match {
        case Nil => Iterator.empty
        case head +: Nil => head.map(Seq(_))
        case head +: tail => new CombineTwo(head, new CombineSeq(tail)).map(t => t._1 +: t._2)
      }

      /** Iterator which combines two iterators (return all combinations of their results).
        *
        * @param iter1 First iterator.
        * @param given Second iterator.
        * @tparam A The first type
        * @tparam B The second type
        */
      private class CombineTwo[A, B](iter1: Iterator[A], given: Iterator[B]) extends AbstractIterator[(A, B)] {


        /* --- AbstractIterator Implementation --- */

        override def hasNext: Boolean = (iter1.hasNext && kept.hasNext) || // can reload
          (cached.isDefined && iter2.hasNext) // reload is readable

        override def next(): (A, B) = {
          if (!iter2.hasNext) { // Reload
            this.cached = Some(iter1.next())
            val two = kept.duplicate
            this.kept = two._1
            this.iter2 = two._2
          }
          (cached.get, iter2.next())
        }


        /* --- Privates --- */

        /** Kept holds the last state.
          * iter2 holds the current second state.
          */
        private var (kept, iter2) = given.duplicate

        /** Cached  last first result. */
        private var cached: Option[A] = if (iter1.hasNext) {
          Some(iter1.next)
        } else {
          None
        }

      }

    }

    /** Build iterator of program's trees where their root is the current target.
      *
      * @param root The root of the programs we find
      * @return Iterator with all the programs of root.
      */
    def recursive(root: HyperTerm): Iterator[Tree[Int]] = if (targetToEdges.contains(root)) {
      targetToEdges(root).toIterator.flatMap(edge => {
        new CombineSeq(edge.sources.map(recursive)).map(subtrees => new Tree[Int](edge.edgeType.id, subtrees.toList))
      })
    } else {
      Iterator(new Tree[Int](root.id))
    }

    if (hyperGraph.edgeTypes.contains(hyperTerm)) {
      hyperGraph.edges.toIterator.filter(_.edgeType == hyperTerm).map(_.target).flatMap(recursive)
    } else {
      Iterator(new Tree[Int](hyperTerm.id))
    }
  }
}

object Programs extends LazyLogging {


  /* --- Private --- */

  private def destruct(tree: Tree[Int]): HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm] = {
    logger.trace("Destruct a program")

    def allIds(tree: Tree[Int]): Set[Int] = {
      tree.subtrees.flatMap(allIds).toSet + tree.root
    }

    def destruct(tree: Tree[Int], counter: () => Int): Set[HyperEdge[HyperTerm, HyperTerm]] = {
      if (tree.isLeaf) {
        Set.empty
      } else {
        val newHyperEdge = HyperEdge[HyperTerm, HyperTerm](HyperTerm(counter()), HyperTerm(tree.root), tree.subtrees.map(_.root).map(HyperTerm))
        val subHyperEdges = tree.subtrees.flatMap(subtree => destruct(subtree, counter)).toSet
        subHyperEdges + newHyperEdge
      }
    }

    val hyperEdges = destruct(tree, Stream.from(allIds(tree).max + 1).iterator.next)

    hyperEdges.foldLeft[HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm]](new VocabularyHyperGraph[HyperTerm, HyperTerm]())((graph, edge)=>graph.addEdge(edge))
  }
}