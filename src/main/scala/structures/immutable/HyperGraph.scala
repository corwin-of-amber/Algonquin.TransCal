package structures.immutable

import structures.{HyperEdge, HyperGraphLike, Item}

import scala.collection.immutable


trait HyperGraph[Node, EdgeType]
  extends immutable.Set[HyperEdge[Node, EdgeType]]
     with HyperGraphLike[Node, EdgeType, HyperGraph[Node, EdgeType]] {

  override def empty: HyperGraph[Node, EdgeType] = HyperGraph.empty

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @return The matched references.
    */
  def findSubgraph[Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]]): Set[(Map[Id, Node], Map[Id, EdgeType])] =
    findSubgraph[Id, HyperGraph[Item[Node, Id], Item[EdgeType, Id]]](hyperPattern)
}

object HyperGraph extends HyperGraphLikeGenericCompanion[HyperGraph] {

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: collection.mutable.Builder[HyperEdge[A, B], HyperGraph[A, B]] =
    VocabularyHyperGraph.newBuilder

}
