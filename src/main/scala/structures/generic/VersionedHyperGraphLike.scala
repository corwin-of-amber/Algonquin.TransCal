package structures.generic

import structures.generic.HyperGraph.HyperGraphPattern
import structures.{HyperEdge, HyperGraphLike, Item}

trait VersionedHyperGraphLike[Node, EdgeType, +This <: VersionedHyperGraphLike[Node, EdgeType, This] with scala.collection.Set[structures.HyperEdge[Node,EdgeType]]] extends HyperGraphLike[Node, EdgeType, This]{
  protected def getHyperGraph: HyperGraph[Node, EdgeType]
  protected def getLastVersion: HyperGraph[Node, EdgeType]

  def isLatest(hyperEdge: HyperEdge[Node, EdgeType]): Boolean = getLastVersion.contains(hyperEdge)

  protected def swapNode(hyperEdge: HyperEdge[Node, EdgeType], keep: Node, change: Node): HyperEdge[Node, EdgeType] =
    hyperEdge.copy(target = if (hyperEdge.target == change) keep else hyperEdge.target, sources = hyperEdge.sources.map(s => if (s == change) keep else s))

  protected def swapEdgeType(hyperEdge: HyperEdge[Node, EdgeType], keep: EdgeType, change: EdgeType): HyperEdge[Node, EdgeType] =
    hyperEdge.copy(edgeType = if (hyperEdge.edgeType == change) keep else hyperEdge.edgeType)

  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id]): Set[HyperGraph.Match[Node, EdgeType, Id]] = {
    (for (
      edgePattern <- hyperPattern;
      edge <- getLastVersion.findRegexHyperEdges(edgePattern)
    ) yield {
      val nodes = (edgePattern.target +: edgePattern.sources).zip(edge.target +: edge.sources)
      val edgeTypes = Seq((edgePattern.edgeType, edge.edgeType))
      val nodesMap = Item.itemsValueToMap(nodes)
      val edgeTypeMap = Item.itemsValueToMap(edgeTypes)
      val matched = HyperGraph.Match(Set(edge), nodesMap, edgeTypeMap)
      val g = structures.generic.HyperGraph.mergeMatch(hyperPattern, matched)
      getHyperGraph.findSubgraph[Id](g).map { m => m.merge(matched) }
    }).flatten.toSet
  }

  def resetVersion(): This
  def addKeepVersion(hyperEdge: HyperEdge[Node, EdgeType]): This
  def addAllKeepVersion(hyperEdges: Set[HyperEdge[Node, EdgeType]]): This
  def mergeNodesKeepVersion(keep: Node, change: Node): This
  def mergeEdgeTypesKeepVersion(keep: EdgeType, change: EdgeType): This
}

object VersionedHyperGraphLike {
  val STATIC_VERSION = 0L
}
