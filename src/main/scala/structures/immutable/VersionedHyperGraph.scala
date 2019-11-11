package structures.immutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import structures.generic.{HyperGraphLikeGenericCompanion, VersionedHyperGraphLike}

import scala.collection.{GenTraversableOnce, mutable}

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: VocabularyHyperGraph[Node, EdgeType], val version: Long, mapByVersion: Map[Long, HyperGraph[Node, EdgeType]])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped)
    with VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]] {

  /* --- Constructors --- */

  def this(wrapped: VocabularyHyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionedHyperGraphLike.STATIC_VERSION + 1, Map(VersionedHyperGraphLike.STATIC_VERSION -> wrapped))
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
      val newMapByVersion = mapByVersion + ((version, mapByVersion.getOrElse(version, HyperGraph.empty) + hyperEdge))
      new VersionedHyperGraph(
        wrapped.+(hyperEdge),
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
    val newEdges = hyperEdges.toSeq.filterNot(contains)
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
    val diff = findByEdgeType(change).map(e => swapEdgeType(e, keep, change))
    // Handle mapByVersion
    val newMapByVersion = {
      val tempNewMapByVersion = mapByVersion.mapValues(_.mergeEdgeTypes(keep, change) -- diff)
      tempNewMapByVersion.updated(version, tempNewMapByVersion.getOrElse(version, HyperGraph.empty) ++ diff)
    }
    // Handle wrapped
    val newWrapped = wrapped.mergeEdgeTypes(keep, change)
    // Handle version
    new VersionedHyperGraph(
      newWrapped,
      version + 1,
      newMapByVersion
    )
  }

  override def mergeNodes(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findInNodes(change).map(e => swapNode(e, keep, change))
    // Handle mapByVersion
    val newMapByVersion = {
      val tempNewMapByVersion = mapByVersion.mapValues(_.mergeNodes(keep, change) -- diff)
      tempNewMapByVersion.updated(version, tempNewMapByVersion.getOrElse(version, HyperGraph.empty) ++ diff)
    }
    // Handle wrapped
    val newWrapped = wrapped.mergeNodes(keep, change)
    // Handle version
    new VersionedHyperGraph(
      newWrapped,
      version + 1,
      newMapByVersion
    )
  }

  /* --- Object Impl. --- */

  override lazy val toString: String = f"VersionedHyperGraph($version, $wrapped)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        val innerHyperGraph = VocabularyHyperGraph(parts: _*)
        val newMapByVersion = mapByVersion + ((version, mapByVersion.getOrElse(version, HyperGraph.empty) ++ innerHyperGraph))
        new VersionedHyperGraph(innerHyperGraph, version + 1, newMapByVersion)
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
}
