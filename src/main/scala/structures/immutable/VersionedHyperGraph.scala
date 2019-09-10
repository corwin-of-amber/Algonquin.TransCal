package structures.immutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.VersionedHyperGraph.VersionMetadata

import scala.collection.{GenTraversableOnce, mutable}

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: HyperGraph[Node, EdgeType], val version: Long, mapByVersion: Map[Long, HyperGraph[Node, EdgeType]])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped) {

  /* --- Constructors --- */

  def this(wrapped: HyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionMetadata.latestVersion(wrapped.edges), VersionedHyperGraph.createMapOfVersions(wrapped))
  }

  /* --- Public Methods --- */

  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    if (version == 0) wrapped.findSubgraph[Id](hyperPattern)
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
  override def +(hyperEdge: HyperEdge[Node, EdgeType]): VersionedHyperGraph[Node, EdgeType] = {
    if (contains(hyperEdge)) this
    else {
      val newHyperEdge = hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version)))
      val newMapByVersion = mapByVersion + ((version, mapByVersion.getOrElse(version, HyperGraph.empty) + newHyperEdge))
      new VersionedHyperGraph(
        wrapped.+(newHyperEdge),
        version + 1,
        newMapByVersion
      )
    }
  }

  /**
    * Adding version to edges.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edge.
    */
  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): VersionedHyperGraph[Node, EdgeType] = {
    val newEdges = hyperEdges.toSeq.filter(contains).map(hyperEdge => hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version))))
    if (newEdges.isEmpty) this
    else {
      val newMapByVersion = mapByVersion + ((version, mapByVersion.getOrElse(version, HyperGraph.empty) ++ newEdges))
      new VersionedHyperGraph(
        wrapped.++(newEdges),
        version + 1,
        newMapByVersion
      )
    }
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    val newMapByVersion = mapByVersion.mapValues(_.mergeEdgeTypes(keep, change))
    val diff = newMapByVersion.map{case (version, graph) => mapByVersion(version) -- graph}.foldLeft(HyperGraph.empty[Node, EdgeType])(_ ++ _).mergeEdgeTypes(keep, change)
    val newMapByVersion1 = newMapByVersion.+((version, newMapByVersion.getOrElse(version, HyperGraph.empty) ++ diff))
    new VersionedHyperGraph(
      wrapped.mergeEdgeTypes(keep, change),
      version + 1,
      newMapByVersion1
    )
  }

  override def mergeNodes(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    val newMapByVersion = mapByVersion.mapValues(_.mergeNodes(keep, change))
    val diff = newMapByVersion.map{case (version, graph) => mapByVersion(version) -- graph}.foldLeft(HyperGraph.empty[Node, EdgeType])(_ ++ _).mergeNodes(keep, change)
    val newMapByVersion1 = newMapByVersion.updated(version, newMapByVersion.getOrElse(version, HyperGraph.empty) ++ diff)
    new VersionedHyperGraph(
      wrapped.mergeNodes(keep, change),
      version + 1,
      newMapByVersion1
    )
  }

  /* --- Object Impl. --- */

  override lazy val toString: String = f"VersionedHyperGraph($version, $wrapped)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        val innerHyperGraph = HyperGraph(parts:_*)
        val newMapByVersion = mapByVersion + ((version, mapByVersion.getOrElse(version, HyperGraph.empty) ++ innerHyperGraph))
        new VersionedHyperGraph(innerHyperGraph, version + 1, newMapByVersion)
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

  private def createMapOfVersions[Node, EdgeType](edges: HyperGraph[Node, EdgeType]): Map[Long, HyperGraph[Node, EdgeType]] =
    edges.groupBy(edge => VersionMetadata.getEdgeVersion(edge)).mapValues(g => HyperGraph(g.toSeq:_*))
}
