package structures.mutable

import structures.generic.HyperGraphLikeGenericCompanion
import structures.{Explicit, Hole, HyperEdge, Item}


trait HyperGraph[Node, EdgeType]
  extends scala.collection.mutable.Set[HyperEdge[Node, EdgeType]]
    with structures.generic.HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, HyperGraph[Node, EdgeType]] {

  override def empty: HyperGraph[Node, EdgeType] = HyperGraph.empty
}

object HyperGraph extends HyperGraphLikeGenericCompanion[HyperGraph] {

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: collection.mutable.Builder[HyperEdge[A, B], HyperGraph[A, B]] =
    VocabularyHyperGraph.newBuilder

  def mergeMap[Node, EdgeType, Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]], maps: (Map[Id, Node], Map[Id, EdgeType])): HyperGraph[Item[Node, Id], Item[EdgeType, Id]] = {
    val (nodeMap, edgeMap) = maps
    val mergedNodes = nodeMap.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodesInPlace(Explicit[Node, Id](kv._2), Hole[Node, Id](kv._1))
    })

    val mergedAll = edgeMap.foldLeft(mergedNodes)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeEdgeTypesInPlace(Explicit[EdgeType, Id](kv._2), Hole[EdgeType, Id](kv._1))
    })
    mergedAll
  }

  def fillWithNewHoles[Node, EdgeType, Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]], nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    val newTerms = hyperPattern.nodes.filter(_.isInstanceOf[Hole[Node, Id]]).map((_, nodeCreator()))
    newTerms.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodesInPlace(Explicit[Node, Id](kv._2), kv._1)
    }).map(translateEdge).toSet
  }
}
