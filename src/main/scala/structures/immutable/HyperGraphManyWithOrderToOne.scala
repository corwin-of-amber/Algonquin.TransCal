package structures.immutable

import structures.immutable.HyperGraphManyWithOrderToOneLike.HyperEdgePattern
import structures.{Explicit, Hole, HyperEdge, Item}

import scala.collection.{immutable, mutable}

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

  def mergeMap[Node, EdgeType, Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id]]
  (hyperPattern: Pattern, maps: (Map[Id, Node], Map[Id, EdgeType])): Pattern = {
    val (nodeMap, edgeMap) = maps
    val mergedNodes = nodeMap.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodes(Explicit[Node, Id](kv._2), Hole[Node, Id](kv._1)).asInstanceOf[Pattern]
    })

    val mergedAll = edgeMap.foldLeft(mergedNodes)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeEdgeTypes(Explicit[EdgeType, Id](kv._2), Hole[EdgeType, Id](kv._1)).asInstanceOf[Pattern]
    })
    mergedAll
  }

  def fillWithNewHoles[Node, EdgeType, Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id],
                                           nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    val newTerms = hyperPattern.nodes.filter(_.isInstanceOf[Hole[Node, Id]]).map((_, nodeCreator()))

    def translateEdge(hyperEdgePattern: HyperEdgePattern[Node, EdgeType, Id]): HyperEdge[Node, EdgeType] = {
      hyperEdgePattern.copy(hyperEdgePattern.target.asInstanceOf[Explicit[Node, Id]].value, hyperEdgePattern.edgeType.asInstanceOf[Explicit[EdgeType, Id]].value,
        hyperEdgePattern.sources.map(s => s.asInstanceOf[Explicit[Node, Id]].value))
    }

    newTerms.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodes(Explicit[Node, Id](kv._2), kv._1)
    }).map(translateEdge(_))
  }

  def fillPattern[Node, EdgeType, Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], maps: (Map[Id, Node], Map[Id, EdgeType]), nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    // Should crash if we still have holes as its a bug
    val mergedAll = mergeMap(hyperPattern, maps)
    fillWithNewHoles(mergedAll, nodeCreator)
  }
}