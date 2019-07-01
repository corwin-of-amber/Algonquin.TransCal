package structures.immutable

import structures.{HyperEdge, Item}

import scala.collection.{mutable, immutable}

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType] extends immutable.Set[HyperEdge[Node, EdgeType]] with HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]] {

  override def empty: HyperGraphManyWithOrderToOne[Node, EdgeType] = HyperGraphManyWithOrderToOne.empty

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @return The matched references.
    */
  def findSubgraph[Id](hyperPattern: HyperGraphManyWithOrderToOne[Item[Node, Id], Item[EdgeType, Id]]): Set[(Map[Id, Node], Map[Id, EdgeType])] =
    findSubgraph[Id, HyperGraphManyWithOrderToOne[Item[Node, Id], Item[EdgeType, Id]]](hyperPattern)
}

object HyperGraphManyWithOrderToOne extends HyperGraphManyWithOrderToOneLikeGenericCompanion[HyperGraphManyWithOrderToOne] {
  type HyperGraphPattern[Node, EdgeType, Id] = HyperGraphManyWithOrderToOne[Item[Node, Id], Item[EdgeType, Id]]

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], HyperGraphManyWithOrderToOne[A, B]] =
    VocabularyHyperGraph.newBuilder
}
