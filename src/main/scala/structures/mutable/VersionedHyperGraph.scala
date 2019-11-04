package structures.mutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.HyperGraphLikeGenericCompanion
import structures.generic.VersionedHyperGraph.VersionMetadata

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: VocabularyHyperGraph[Node, EdgeType], var version: Long, mapByVersion: mutable.Map[Long, HyperGraph[Node, EdgeType]])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped) {

  private var locked: Boolean = false
  private var versionRaised: Boolean = false

  /* --- Constructors --- */

  def this(wrapped: VocabularyHyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionMetadata.latestVersion(wrapped.edges), VersionedHyperGraph.createMapOfVersions(wrapped))
  }

  /* --- Public Methods --- */

  override def clone = new VersionedHyperGraph[Node, EdgeType](wrapped.clone, version, mutable.Map.empty)

  def lockVersion(): Unit = {
    locked = true
  }
  def unlockVersion(): Unit = {
    locked = false
    versionRaised = false
  }

  def incSelfVersion(): Unit = {
    if (!locked)
      version += 1
    else if (!versionRaised) {
      versionRaised = true
      version += 1
    }
  }

  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    if (locked) throw new RuntimeException("Can't use find versioned while versions are locked")
    if (version == 0) wrapped.findSubgraph[Id](hyperPattern)
    else (for (
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

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (!contains(hyperEdge)) {
      incSelfVersion()
      val newEdge = hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version)))
      wrapped += newEdge
      mapByVersion.getOrElseUpdate(version, HyperGraph.empty) += newEdge
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
      incSelfVersion()
      wrapped ++= newEdges
      mapByVersion.getOrElseUpdate(version, HyperGraph.empty) ++= newEdges
    }
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    def swap(hyperEdge: HyperEdge[Node, EdgeType]): HyperEdge[Node, EdgeType] =
      hyperEdge.copy(edgeType = if (hyperEdge.edgeType == change) keep else hyperEdge.edgeType)
    def updateVersion(hyperEdge: HyperEdge[Node, EdgeType]): HyperEdge[Node, EdgeType] =
      hyperEdge.copy(metadata = UnionMetadata(hyperEdge.metadata.filterNot(_.isInstanceOf[VersionMetadata]).toSet).merge(VersionMetadata(version)))
    incSelfVersion()
    val diff = findByEdgeType(change).map(swap).map(updateVersion)
//     Handle mapByVersion
    for (value <- mapByVersion.values) {
      value.mergeEdgeTypesInPlace(keep, change) --= diff
    }
    mapByVersion.update(version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= diff)
    // Handle wrapped
    wrapped.mergeEdgeTypesInPlace(keep, change)
    for(e <- diff) {
      wrapped.updateMetadataInPlace(e)
    }
    // Handle version
    this
  }

  override def mergeNodesInPlace(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    def swap(hyperEdge: HyperEdge[Node, EdgeType]): HyperEdge[Node, EdgeType] =
      hyperEdge.copy(target = if (hyperEdge.target == change) keep else hyperEdge.target, sources = hyperEdge.sources.map(s => if (s == change) keep else s))
    def updateVersion(hyperEdge: HyperEdge[Node, EdgeType]): HyperEdge[Node, EdgeType] =
      hyperEdge.copy(metadata = UnionMetadata(hyperEdge.metadata.filterNot(_.isInstanceOf[VersionMetadata]).toSet).merge(VersionMetadata(version)))
    incSelfVersion()
    val diff = findInNodes(change).map(swap).map(updateVersion)
    // Handle mapByVersion
    for (value <- mapByVersion.values) {
      value.mergeNodesInPlace(keep, change) --= diff
    }
    mapByVersion.update(version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= diff)
    // Handle wrapped
    wrapped.mergeNodesInPlace(keep, change)
    for(e <- diff) {
      wrapped.updateMetadataInPlace(e)
    }

    // Handle version
    this
  }

  /* --- Object Impl. --- */

  override def toString: String = f"VersionedHyperGraph($version, $wrapped)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        new VersionedHyperGraph(VocabularyHyperGraph(parts:_*))
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
      new VersionedHyperGraph(VocabularyHyperGraph(parts: _*))
    }
  }

  private def createMapOfVersions[Node, EdgeType](edges: HyperGraph[Node, EdgeType]): mutable.Map[Long, HyperGraph[Node, EdgeType]] = {
    mutable.Map.apply(edges.groupBy(edge => VersionMetadata.getEdgeVersion(edge)).mapValues(g => HyperGraph(g.toSeq: _*)).toSeq: _*)
//    mutable.Map()
  }
}
