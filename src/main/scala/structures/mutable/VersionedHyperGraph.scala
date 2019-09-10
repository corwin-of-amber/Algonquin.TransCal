package structures.mutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.VersionedHyperGraph.VersionMetadata

import scala.collection.{GenTraversableOnce, mutable}

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
      val relevantVersionGraph = HyperGraph(mapByVersion.filterKeys(_ >= version).values.flatten.toSeq:_*)
      hyperPattern.flatMap((edgePattern: HyperEdge[Item[Node, Id], Item[EdgeType, Id]]) => {
        val rightVersionEdges = relevantVersionGraph.findRegexHyperEdges(edgePattern)
        rightVersionEdges.flatMap((edge: HyperEdge[Node, EdgeType]) => {
          val nodes = (edgePattern.target +: edgePattern.sources).zip(edge.target +: edge.sources)
          val edgeTypes = Seq((edgePattern.edgeType, edge.edgeType))
          val nodesMap = Item.itemsValueToMap(nodes)
          val edgeTypeMap = Item.itemsValueToMap(edgeTypes)
          val g = structures.generic.HyperGraph.mergeMap(hyperPattern, (nodesMap, edgeTypeMap))
          wrapped.findSubgraph[Id](g).map { case (foundNodes: Map[Id, Node], foundEdgeType: Map[Id, EdgeType]) => (foundNodes ++ nodesMap, foundEdgeType ++ edgeTypeMap) }
        })
      })
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
      val newMapByVersion = mapByVersion.clone()
      newMapByVersion.getOrElseUpdate(version, HyperGraph.empty) += newHyperEdge
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
      val newMapByVersion = mapByVersion.clone()
      mapByVersion.getOrElseUpdate(version, HyperGraph.empty) ++= newEdges
      new VersionedHyperGraph(
        wrapped.++(newEdges),
        version + 1,
        newMapByVersion
      )
    }
  }

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): Unit = {
    if (!contains(hyperEdge)) {
      val newEdge = hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version)))
      wrapped += newEdge
      mapByVersion.getOrElseUpdate(version, HyperGraph.empty) += newEdge
      this.version += 1
    }
  }

  /**
    * Adding version to edges.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edge.
    */
  override def ++=(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): Unit = {
    val newEdges = hyperEdges.toSeq.filter(contains).map(hyperEdge => hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version))))
    if (newEdges.nonEmpty) {
      wrapped ++= newEdges
      mapByVersion.getOrElseUpdate(version, HyperGraph.empty) ++= newEdges
      this.version += 1
    }
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    for (value <- mapByVersion.values) {
      value.mergeEdgeTypes(keep, change)
    }
    val diff = (wrapped --  mapByVersion.values.foldLeft(HyperGraph.empty[Node, EdgeType])(_ ++ _)).mergeEdgeTypes(keep, change)
    mapByVersion.update(version, mapByVersion.getOrElse(version, HyperGraph.empty) ++ diff)
    wrapped.mergeEdgeTypes(keep, change)
    version += 1
    this
  }

  override def mergeNodes(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    for (value <- mapByVersion.values) {
      value.mergeNodes(keep, change)
    }
    val diff = (wrapped --  mapByVersion.values.foldLeft(HyperGraph.empty[Node, EdgeType])(_ ++ _)).mergeNodes(keep, change)
    mapByVersion.update(version, mapByVersion.getOrElse(version, HyperGraph.empty) ++ diff)
    wrapped.mergeNodes(keep, change)
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

  private def createMapOfVersions[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): mutable.Map[Long, HyperGraph[Node, EdgeType]] = {
    mutable.Map.apply(edges.groupBy(edge => VersionMetadata.getEdgeVersion(edge)).mapValues(g => HyperGraph(g.toSeq: _*)).toSeq: _*)
  }
}
