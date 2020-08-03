package structures.mutable

import structures.{Explicit, Hole, HyperEdge, Item}


trait HyperGraph[Node, EdgeType]
  extends scala.collection.mutable.Set[HyperEdge[Node, EdgeType]]
    with structures.HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, HyperGraph[Node, EdgeType]] {

  override def empty: HyperGraph[Node, EdgeType] = HyperGraph.empty
}

object HyperGraph extends HyperGraphCompanion[HyperGraph] {

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: collection.mutable.Builder[HyperEdge[A, B], HyperGraph[A, B]] =
    VocabularyHyperGraph.newBuilder
}
