package synthesis

import structures.HyperGraphManyWithOrderToOne
import structures.mutable.CombineSeq
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
}
