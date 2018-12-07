package structures.immutable

import structures.HyperGraphManyWithOrderToOneLike

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType] extends structures.HyperGraphManyWithOrderToOne[Node, EdgeType]
  with HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]]

object HyperGraphManyWithOrderToOne {
  def empty[Node, EdgeType]: HyperGraphManyWithOrderToOne[Node, EdgeType] = structures.immutable.VocabularyHyperGraph.empty
}