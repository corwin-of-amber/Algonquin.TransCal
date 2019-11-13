package structures.mutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.{HyperGraphLikeGenericCompanion, VersionedHyperGraphLike}

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: VocabularyHyperGraph[Node, EdgeType], var version: Long, var mapByVersion: Map[Long, HyperGraph[Node, EdgeType]], var locked: Boolean)
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped)
    with VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]] {

  /* --- Constructors --- */

  def this(wrapped: VocabularyHyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionedHyperGraphLike.STATIC_VERSION + 1, Map(VersionedHyperGraphLike.STATIC_VERSION -> wrapped.clone), false)
  }

  /* --- Public Methods --- */

  override def clone = new VersionedHyperGraph[Node, EdgeType](wrapped.clone, version, mapByVersion.mapValues(g => g.clone), isLocked)

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (!contains(hyperEdge)) {
      wrapped += hyperEdge
      mapByVersion = mapByVersion updated (version, mapByVersion.getOrElse(version, HyperGraph.empty) += hyperEdge)
      if (!isLocked)
        version += 1
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
    val newEdges = hyperEdges.toSeq.filterNot(contains)
    if (newEdges.nonEmpty) {
      wrapped ++= newEdges
      mapByVersion = mapByVersion updated (version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= newEdges)
      if (!isLocked)
        version += 1
    }
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findByEdgeType(change).map(e => swapEdgeType(e, keep, change))
    // Handle mapByVersion
    for (value <- mapByVersion.values) {
      value.mergeEdgeTypesInPlace(keep, change) --= diff
    }
    mapByVersion = mapByVersion updated (version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= diff)
    // Handle wrapped
    wrapped.mergeEdgeTypesInPlace(keep, change)
    // Handle version
    if (!isLocked)
      version += 1
    this
  }

  override def mergeNodesInPlace(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findInNodes(change).map(e => swapNode(e, keep, change))
    // Handle mapByVersion
    for (value <- mapByVersion.values) {
      value.mergeNodesInPlace(keep, change) --= diff
    }
    mapByVersion = mapByVersion updated (version, mapByVersion.getOrElse(version, HyperGraph.empty) ++= diff)
    // Handle wrapped
    wrapped.mergeNodesInPlace(keep, change)

    // Handle version
    if (!isLocked)
      version += 1
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

  override def isLocked: Boolean = locked

  override def lockVersions(): VersionedHyperGraph[Node, EdgeType] = clone.lockVersionsInPlace()

  override def unlockVersions(): VersionedHyperGraph[Node, EdgeType] = clone.unlockVersionsInPlace()

  def lockVersionsInPlace(): VersionedHyperGraph[Node, EdgeType] = {
    locked = true;
    this
  }

  def unlockVersionsInPlace(): VersionedHyperGraph[Node, EdgeType] = {
    locked = false;
    if (mapByVersion.contains(version))
      version += 1
    this
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
}
