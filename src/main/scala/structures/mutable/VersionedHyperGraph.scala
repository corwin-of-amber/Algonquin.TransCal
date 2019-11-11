package structures.mutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.{HyperGraphLikeGenericCompanion, VersionedHyperGraphLike}
import structures.generic.VersionedHyperGraphLike.VersionMetadata

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: VocabularyHyperGraph[Node, EdgeType], var version: Long, var mapByVersion: Map[Long, HyperGraph[Node, EdgeType]])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped)
    with VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]] {

  /* --- Constructors --- */

  def this(wrapped: VocabularyHyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionMetadata.latestVersion(wrapped.edges), VersionedHyperGraph.createMapOfVersions(wrapped))
  }

  /* --- Public Methods --- */

  override def clone = new VersionedHyperGraph[Node, EdgeType](wrapped.clone, version, mapByVersion)

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (!contains(hyperEdge)) {
      version += 1
      val newEdge = hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version)))
      wrapped += newEdge
      if (mapByVersion.contains(version)) mapByVersion(version) += newEdge
      else mapByVersion = mapByVersion updated (version, HyperGraph.empty += newEdge)
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
    val newEdges = hyperEdges.toSeq.filterNot(contains).map(updateVersion)
    if (newEdges.nonEmpty) {
      version += 1
      wrapped ++= newEdges
      mapByVersion = mapByVersion updated (version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= newEdges)
    }
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    version += 1
    val diff = findByEdgeType(change).map(e => swapEdgeType(e, keep, change)).map(updateVersion)
//     Handle mapByVersion
    for (value <- mapByVersion.values) {
      value.mergeEdgeTypesInPlace(keep, change) --= diff
    }
    mapByVersion = mapByVersion updated (version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= diff)
    // Handle wrapped
    wrapped.mergeEdgeTypesInPlace(keep, change)
    for(e <- diff) {
      wrapped.updateMetadataInPlace(e)
    }
    // Handle version
    this
  }

  override def mergeNodesInPlace(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    version += 1
    val diff = findInNodes(change).map(e => swapNode(e, keep, change)).map(updateVersion)
    // Handle mapByVersion
    for (value <- mapByVersion.values) {
      value.mergeNodesInPlace(keep, change) --= diff
    }
    mapByVersion = mapByVersion updated (version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= diff)
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

  override protected def getHyperGraph: generic.HyperGraph[Node, EdgeType] = wrapped

  override protected def getVersions: Map[Long, generic.HyperGraph[Node, EdgeType]] = mapByVersion

  override def currentVersion: Long = version
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

  private def createMapOfVersions[Node, EdgeType](edges: HyperGraph[Node, EdgeType]): Map[Long, HyperGraph[Node, EdgeType]] = {
    Map.apply(edges.groupBy(edge => VersionMetadata.getEdgeVersion(edge)).mapValues(g => HyperGraph(g.toSeq: _*)).toSeq: _*)
//    mutable.Map()
  }
}
