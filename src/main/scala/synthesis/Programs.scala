package synthesis

import structures.{HyperEdge, HyperGraphManyWithOrderToOne}
import syntax.Tree

/**
  * @author tomer
  * @since 11/19/18
  */
class Programs(var hyperGraph: HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm]) {


  /* --- Public --- */

  /** Builds trees from of programs where the hyper term is the base program.
    *
    * @param hyperTerm The hyper term to build.
    * @return All the trees.
    */
  def reconstruct(hyperTerm: HyperTerm): Iterator[Tree[Int]] = {
      val targetToEdges = hyperGraph.edges.groupBy(edge=>edge.target)
      def recursive(currentHyperTerm: HyperTerm): Iterator[Tree[Int]] = {
        if (targetToEdges.contains(currentHyperTerm)) {
          targetToEdges(currentHyperTerm).toIterator.flatMap(edge => {
            new CombineSeq(edge.sources.map(recursive)).map(subtrees => new Tree[Int](edge.edgeType.id, subtrees.toList))
          })
        } else {
          Iterator(new Tree[Int](currentHyperTerm.id))
        }
      }
      recursive(hyperTerm)
  }


  /* --- Private --- */

  def destruct(tree: Tree[Int], counter: () => Int): Set[HyperEdge[HyperTerm, HyperTerm]] = {
    if (tree.isLeaf) {
      Set.empty
    } else {
      val newHyperEdge = new HyperEdge[HyperTerm, HyperTerm](PlaceHolder(counter()), Value(tree.root), tree.subtrees.map(subtree => Value(subtree.root)))
      val subHyperEdges = tree.subtrees.flatMap(subtree => destruct(subtree, counter)).toSet
      subHyperEdges + newHyperEdge
    }
  }

  import scala.collection.AbstractIterator

  private class CombineSeq[A](iterators: Seq[Iterator[A]]) extends AbstractIterator[Seq[A]] {


    /* --- AbstractIterator Implementation --- */

    override def hasNext: Boolean = innerIterator.hasNext

    override def next(): Seq[A] = innerIterator.next()


    /* --- Privates --- */

    private val innerIterator: Iterator[Seq[A]] = iterators match {
      case Nil => Iterator.empty
      case head +: Nil => head.map(Seq(_))
      case head +: tail =>  new CombineTwo(head, new CombineSeq(tail)).map(t=>t._1 +: t._2)
    }

    private class CombineTwo[B](iter1: Iterator[A], given: Iterator[B]) extends AbstractIterator[(A, B)] {


      /* --- AbstractIterator Implementation --- */

      override def hasNext: Boolean = (iter1.hasNext && kept.hasNext) || // can reload
        (chached.isDefined && iter2.hasNext)  // reload is readable

      override def next(): (A, B) = {
        if (!iter2.hasNext) { // Reload
          chached = Some(iter1.next())
          (kept, iter2) = kept.duplicate
        }
        (chached.get, iter2.next())
      }


      /* --- Privates --- */

      private var (kept, iter2) = given.duplicate

      private var chached: Option[A] = if (iter1.hasNext) { Some(iter1.next) } else { None }

    }
  }
}