package structures

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType]
  extends HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]] {

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @return The matched references.
    */
  def findSubgraph[Id](hyperPattern: HyperGraphManyWithOrderToOne[Item[Node, Id], Item[EdgeType, Id]]): Set[(Map[Id, Node], Map[Id, EdgeType])] =
    findSubgraph[Id, HyperGraphManyWithOrderToOne[Item[Node, Id], Item[EdgeType, Id]]](hyperPattern)
}
