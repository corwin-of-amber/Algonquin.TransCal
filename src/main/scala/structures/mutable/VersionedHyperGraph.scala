package structures.mutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.VersionedHyperGraph.VersionMetadata

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: HyperGraph[Node, EdgeType], var version: Long, mapByVersion: mutable.Map[Long, HyperGraph[Node, EdgeType]])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped) {

  /* --- Constructors --- */

  def this(wrapped: HyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionMetadata.latestVersion(wrapped.edges), VersionedHyperGraph.createMapOfVersions(wrapped))
  }

  /* --- Public Methods --- */

  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    if (version == 0)
      wrapped.findSubgraph[Id](hyperPattern)
    else {
      (for (
        relevantVersionGraph <- mapByVersion.filterKeys(_ >= version).values;
        edgePattern <- hyperPattern;
        edge <- relevantVersionGraph.findRegexHyperEdges(edgePattern)
      ) yield {
        val nodes = (edgePattern.target +: edgePattern.sources).zip(edge.target +: edge.sources)
        val edgeTypes = Seq((edgePattern.edgeType, edge.edgeType))
        val nodesMap = Item.itemsValueToMap(nodes)
        val edgeTypeMap = Item.itemsValueToMap(edgeTypes)
        val g = structures.generic.HyperGraph.mergeMap(hyperPattern, (nodesMap, edgeTypeMap))
        wrapped.findSubgraph[Id](g).map { case (foundNodes: Map[Id, Node], foundEdgeType: Map[Id, EdgeType]) => (foundNodes ++ nodesMap, foundEdgeType ++ edgeTypeMap) }
      }).flatten.toSet
    }
  }

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (!contains(hyperEdge)) {
      val newEdge = hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version)))
      wrapped += newEdge
      mapByVersion.getOrElseUpdate(version, HyperGraph.empty) += newEdge
      this.version += 1
    }
    this
  }

  /**
    * Adding version to edges.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edge.
    */
  override def ++=(hyperEdges: TraversableOnce[HyperEdge[Node, EdgeType]]): this.type = {
    val newEdges = hyperEdges.toSeq.filter(contains).map(hyperEdge => hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version))))
    if (newEdges.nonEmpty) {
      wrapped ++= newEdges
      mapByVersion.getOrElseUpdate(version, HyperGraph.empty) ++= newEdges
      this.version += 1
    }
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    for (value <- mapByVersion.values) {
      value.mergeEdgeTypesInPlace(keep, change)
    }
    val diff = mapByVersion.values.foldLeft(wrapped)(_ -- _).mergeEdgeTypesInPlace(keep, change)
    mapByVersion.update(version, mapByVersion.getOrElse(version, HyperGraph.empty) ++ diff)
    wrapped.mergeEdgeTypesInPlace(keep, change)
    version += 1
    this
  }

  override def mergeNodesInPlace(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    for (value <- mapByVersion.values) {
      value.mergeNodesInPlace(keep, change)
    }
    val diff = mapByVersion.values.foldLeft(wrapped)(_ -- _).mergeNodesInPlace(keep, change)
    mapByVersion.update(version, mapByVersion.getOrElse(version, HyperGraph.empty) ++ diff)
    wrapped.mergeNodesInPlace(keep, change)
    version += 1
    this
  }

  /* --- Object Impl. --- */

  override def toString: String = f"VersionedHyperGraph($version, $wrapped)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        new VersionedHyperGraph(HyperGraph(parts:_*))
      }
    }
}

object VersionedHyperGraph extends HyperGraphLikeGenericCompanion[VersionedHyperGraph] {
  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], VersionedHyperGraph[A, B]] = new mutable.ListBuffer[HyperEdge[A, B]].mapResult {
    parts => {
      new VersionedHyperGraph(HyperGraph(parts: _*))
    }
  }

  private def createMapOfVersions[Node, EdgeType](edges: HyperGraph[Node, EdgeType]): mutable.Map[Long, HyperGraph[Node, EdgeType]] = {
    mutable.Map.apply(edges.groupBy(edge => VersionMetadata.getEdgeVersion(edge)).mapValues(g => HyperGraph(g.toSeq: _*)).toSeq: _*)
  }
}
