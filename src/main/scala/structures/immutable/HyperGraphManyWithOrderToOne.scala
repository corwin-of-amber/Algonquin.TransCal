package structures.immutable

import structures.HyperGraphManyWithOrderToOneLike
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType] extends structures.HyperGraphManyWithOrderToOne[Node, EdgeType]
  with HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]]

object HyperGraphManyWithOrderToOne {
  def empty[Node, EdgeType]: HyperGraphManyWithOrderToOne[Node, EdgeType] = VocabularyHyperGraph.empty

  def apply[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): HyperGraphManyWithOrderToOne[Node, EdgeType] = VocabularyHyperGraph(edges)
}
