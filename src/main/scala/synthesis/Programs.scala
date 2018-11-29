package synthesis

import structures.HyperEdge
import structures.mutable.{HyperGraphManyWithOrderToOne, VocabularyHyperGraph}
import syntax.Tree

/**
  * @author tomer
  * @since 11/19/18
  */
class Programs(val hyperGraph: HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm]) {


  /* --- Constructors --- */

  def this(tree: Tree[Int]) = this(Programs.destruct(tree))


  /* --- Public --- */

  /** Builds trees from of programs where the hyper term is the base program.
    *
    * @param hyperTerm The hyper term to build.
    * @return All the trees.
    */
  def reconstruct(hyperTerm: HyperTerm): Iterator[Tree[Int]] = {
    val targetToEdges = hyperGraph.edges.groupBy(edge => edge.target)

    import scala.collection.AbstractIterator

    class CombineSeq[A](iterators: Seq[Iterator[A]]) extends AbstractIterator[Seq[A]] {


      /* --- AbstractIterator Implementation --- */

      override def hasNext: Boolean = innerIterator.hasNext

      override def next(): Seq[A] = innerIterator.next()


      /* --- Privates --- */

      private val innerIterator: Iterator[Seq[A]] = iterators match {
        case Nil => Iterator.empty
        case head +: Nil => head.map(Seq(_))
        case head +: tail => new CombineTwo(head, new CombineSeq(tail)).map(t => t._1 +: t._2)
      }

      private class CombineTwo[B](iter1: Iterator[A], given: Iterator[B]) extends AbstractIterator[(A, B)] {


        /* --- AbstractIterator Implementation --- */

        override def hasNext: Boolean = (iter1.hasNext && kept.hasNext) || // can reload
          (chached.isDefined && iter2.hasNext) // reload is readable

        override def next(): (A, B) = {
          if (!iter2.hasNext) { // Reload
            this.chached = Some(iter1.next())
            val two = kept.duplicate
            this.kept = two._1
            this.iter2 = two._2
          }
          (chached.get, iter2.next())
        }


        /* --- Privates --- */

        private var (kept, iter2) = given.duplicate

        private var chached: Option[A] = if (iter1.hasNext) {
          Some(iter1.next)
        } else {
          None
        }

      }

    }

    def recursive(currentHyperTerm: HyperTerm): Iterator[Tree[Int]] = if (targetToEdges.contains(currentHyperTerm)) {
      targetToEdges(currentHyperTerm).toIterator.flatMap(edge => {
        new CombineSeq(edge.sources.map(recursive)).map(subtrees => new Tree[Int](edge.edgeType.id, subtrees.toList))
      })
    } else {
      Iterator(new Tree[Int](currentHyperTerm.id))
    }

    recursive(hyperTerm)
  }
}

object Programs {


  /* --- Private --- */

  private def destruct(tree: Tree[Int]): HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm] = {
    def allIds(tree: Tree[Int]): Set[Int] = {
      tree.subtrees.flatMap(allIds).toSet + tree.root
    }

    def destruct(tree: Tree[Int], counter: () => Int): Set[HyperEdge[HyperTerm, HyperTerm]] = {
      if (tree.isLeaf) {
        Set.empty
      } else {
        val newHyperEdge = new HyperEdge[HyperTerm, HyperTerm](new HyperTerm(counter()), new HyperTerm(tree.root), tree.subtrees.map(subtree => new HyperTerm(subtree.root)))
        val subHyperEdges = tree.subtrees.flatMap(subtree => destruct(subtree, counter)).toSet
        subHyperEdges + newHyperEdge
      }
    }

    val hyperEdges = destruct(tree, Stream.from(allIds(tree).max + 1).iterator.next)

    hyperEdges.foldLeft[HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm]](new VocabularyHyperGraph[HyperTerm, HyperTerm]())((graph, edge)=>graph.addEdge(edge))
  }
}