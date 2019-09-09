package structures.generic

import structures.HyperGraphLike.HyperEdgePattern
import structures.immutable.HyperGraph
import structures.{Explicit, Hole, HyperEdge, Item}

object HyperGraph {
  type HyperGraphPattern[Node, EdgeType, Id] = HyperGraph[Item[Node, Id], Item[EdgeType, Id]]

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
