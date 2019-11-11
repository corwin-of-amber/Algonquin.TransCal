package structures.generic

import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.VersionedHyperGraphLike.VersionMetadata
import structures.{HyperEdge, HyperGraphLike, Item, SpecificMergeMetadata, UnionMetadata}

trait VersionedHyperGraphLike[Node, EdgeType, +This <: VersionedHyperGraphLike[Node, EdgeType, This] with scala.collection.Set[structures.HyperEdge[Node,EdgeType]]] extends HyperGraphLike[Node, EdgeType, This]{
  protected def getHyperGraph: HyperGraph[Node, EdgeType]
  protected def getVersions: collection.Map[Long, HyperGraph[Node, EdgeType]]
  def currentVersion: Long

  protected def updateVersion(hyperEdge: HyperEdge[Node, EdgeType]): HyperEdge[Node, EdgeType] =
    hyperEdge.copy(metadata = UnionMetadata(hyperEdge.metadata.filterNot(_.isInstanceOf[VersionMetadata]).toSet).merge(VersionMetadata(currentVersion)))

  protected def swapNode(hyperEdge: HyperEdge[Node, EdgeType], keep: Node, change: Node): HyperEdge[Node, EdgeType] =
    hyperEdge.copy(target = if (hyperEdge.target == change) keep else hyperEdge.target, sources = hyperEdge.sources.map(s => if (s == change) keep else s))

  protected def swapEdgeType(hyperEdge: HyperEdge[Node, EdgeType], keep: EdgeType, change: EdgeType): HyperEdge[Node, EdgeType] =
    hyperEdge.copy(edgeType = if (hyperEdge.edgeType == change) keep else hyperEdge.edgeType)

  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    if (version == 0) getHyperGraph.findSubgraph[Id](hyperPattern)
    (for (
      relevantVersionGraph <- getVersions.filterKeys(_ >= version).values;
      edgePattern <- hyperPattern;
      edge <- relevantVersionGraph.findRegexHyperEdges(edgePattern)
    ) yield {
      val nodes = (edgePattern.target +: edgePattern.sources).zip(edge.target +: edge.sources)
      val edgeTypes = Seq((edgePattern.edgeType, edge.edgeType))
      val nodesMap = Item.itemsValueToMap(nodes)
      val edgeTypeMap = Item.itemsValueToMap(edgeTypes)
      val g = structures.generic.HyperGraph.mergeMap(hyperPattern, (nodesMap, edgeTypeMap))
      getHyperGraph.findSubgraph[Id](g).map { case (foundNodes: Map[Id, Node], foundEdgeType: Map[Id, EdgeType]) => (foundNodes ++ nodesMap, foundEdgeType ++ edgeTypeMap) }
    }).flatten.toSet
  }
}

object VersionedHyperGraphLike {
  val STATIC_VERSION = 0L

  case class VersionMetadata(version: Long) extends SpecificMergeMetadata {
    override protected def toStr: String = s"VersionMetadata($version)"

    override def mergeSpecific(other: SpecificMergeMetadata): SpecificMergeMetadata = other match {
      case metadata: VersionMetadata => if (metadata.version < version) other else this
    }
  }

  object VersionMetadata {
    /**
      *
      * @param edges To check with
      * @tparam Node     node type
      * @tparam EdgeType edge type
      * @return
      */
    def latestVersion[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): Long = (edges.map(getEdgeVersion) + 0L).min

    /**
      * Getting the edge version
      *
      * @param edge Edge to check with
      * @tparam Node     node type
      * @tparam EdgeType edge type
      * @return
      */
    def getEdgeVersion[Node, EdgeType](edge: HyperEdge[Node, EdgeType]): Long = {
      edge.metadata.collectFirst { case v: VersionedHyperGraphLike.VersionMetadata => v.version }.getOrElse(0L)
    }
  }

}
