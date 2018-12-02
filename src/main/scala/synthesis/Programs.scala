package synthesis

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge
import structures.mutable.VocabularyHyperGraph
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import synthesis.rewrites.RewriteSearchState

/** Programs contains all the available programs holding them for future optimized rewrites and reconstruction of them.
  * @author tomer
  * @since 11/19/18
  */
class Programs(val hyperGraph: RewriteSearchState.HyperGraph) extends LazyLogging {


  /* --- Constructors --- */

  def this(tree: Term) = this(Programs.destruct(tree))


  /* --- Public --- */

  /** Builds trees from of programs where the hyper term is the base program.
    *
    * @param hyperTerm The hyper term to build.
    * @return All the trees.
    */
  def reconstruct(hyperTerm: HyperTerm): Iterator[Term] = {
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
    def recursive(root: HyperTerm): Iterator[Term] = {
      root match {
        case HyperTermId(_) => targetToEdges(root).toIterator.flatMap(edge => {
          new CombineSeq(edge.sources.map(recursive)).map(subtrees => new Tree[Identifier](edge.edgeType.identifier, subtrees.toList))
        })
        case HyperTermIdentifier(identifier) => Iterator(new Tree[Identifier](identifier))
      }
    }

    hyperTerm match {
      case HyperTermId(_) => hyperGraph.edges.toIterator.filter(_.edgeType == hyperTerm).map(_.target).flatMap(recursive)
      case HyperTermIdentifier(identifier) => Iterator(new Tree[Identifier](identifier))
    }
  }
}

object Programs extends LazyLogging {

  /* --- Private --- */

  private def destruct(tree: Term): RewriteSearchState.HyperGraph = {
    logger.trace("Destruct a program")

    def destruct(tree: Term, counter: () => Int): Set[HyperEdge[HyperTerm, HyperTermIdentifier]] = {
      if (tree.isLeaf) {
        Set.empty
      } else {
        val newHyperEdge = HyperEdge[HyperTerm, HyperTermIdentifier](HyperTermId(counter()), Identifier(tree.root), tree.subtrees.map(_.root).map(HyperTermIdentifier))
        val subHyperEdges = tree.subtrees.flatMap(subtree => destruct(subtree, counter)).toSet
        subHyperEdges + newHyperEdge
      }
    }

    val hyperEdges = destruct(tree, Stream.from(1).iterator.next)

    hyperEdges.foldLeft[RewriteSearchState.HyperGraph](new VocabularyHyperGraph[HyperTerm, HyperTermIdentifier]())((graph, edge)=>graph.addEdge(edge))
  }
}