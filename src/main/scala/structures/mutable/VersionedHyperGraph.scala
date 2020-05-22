package structures.mutable

import structures._
import structures.generic.{HyperGraphLikeGenericCompanion, VersionedHyperGraphLike}

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class  VersionedHyperGraph[Node, EdgeType] private(wrapped: VocabularyHyperGraph[Node, EdgeType], var lastVersion: VocabularyHyperGraph[Node, EdgeType])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped)
    with VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]] {

  /* --- Constructors --- */

  def this(wrapped: VocabularyHyperGraph[Node, EdgeType]) = {
    this(wrapped.clone, wrapped.clone)
  }

  /* --- Public Methods --- */

  override def clone = new VersionedHyperGraph[Node, EdgeType](wrapped.clone, lastVersion.clone)

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (!contains(hyperEdge)) {
      lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
      addKeepVersionInPlace(hyperEdge)
    }
    this
  }

  private def innerAddAll(hyperEdges: TraversableOnce[HyperEdge[Node, EdgeType]]) = {
    val newEdges = hyperEdges.toSeq.filterNot(contains)
    if (newEdges.nonEmpty) {
      wrapped ++= newEdges
    }
    newEdges
  }

  /**
    * Adding version to edges.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edge.
    */
  override def ++=(hyperEdges: TraversableOnce[HyperEdge[Node, EdgeType]]): this.type = {
    lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
    lastVersion ++= innerAddAll(hyperEdges)
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): this.type = {
    lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
    mergeEdgeTypesKeepVersionInPlace(keep, change)
  }

  override def mergeNodesInPlace(keep: Node, change: Node): this.type = {
    lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
    mergeNodesKeepVersionInPlace(keep, change)
  }

  /* --- Object Impl. --- */

  override def toString: String = f"VersionedHyperGraph($wrapped, $lastVersion)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        new VersionedHyperGraph(VocabularyHyperGraph(parts:_*))
      }
    }

  override protected def getHyperGraph: generic.HyperGraph[Node, EdgeType] = wrapped

  override def getLastVersion: generic.HyperGraph[Node, EdgeType] = lastVersion

  override def addKeepVersion(hyperEdge: HyperEdge[Node, EdgeType]): VersionedHyperGraph[Node, EdgeType] =
    clone.addKeepVersionInPlace(hyperEdge)

  override def addAllKeepVersion(hyperEdges: Set[HyperEdge[Node, EdgeType]]): VersionedHyperGraph[Node, EdgeType] =
    clone.addAllKeepVersionInPlace(hyperEdges)

  override def mergeNodesKeepVersion(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] =
    clone.mergeNodesKeepVersionInPlace(keep, change)

  override def mergeEdgeTypesKeepVersion(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] =
    mergeEdgeTypesKeepVersionInPlace(keep, change)

  def addKeepVersionInPlace(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (!contains(hyperEdge)) {
      wrapped += hyperEdge
      lastVersion += hyperEdge
    }
    this
  }

  def addAllKeepVersionInPlace(hyperEdges: Set[HyperEdge[Node, EdgeType]]): this.type = {
    lastVersion ++= innerAddAll(hyperEdges)
    this
  }

  def mergeNodesKeepVersionInPlace(keep: Node, change: Node): this.type = {
    val diff = findInNodes(change).map(e => swapNode(e, keep, change))
    // Handle wrapped
    wrapped.mergeNodesInPlace(keep, change)
    // Handle version
    lastVersion.mergeNodesInPlace(keep, change)
    lastVersion ++= diff
    assert(diff.nonEmpty)
    this
  }

  def mergeEdgeTypesKeepVersionInPlace(keep: EdgeType, change: EdgeType): this.type = {
    val diff = findByEdgeType(change).map(e => swapEdgeType(e, keep, change))
    // Handle wrapped
    wrapped.mergeEdgeTypesInPlace(keep, change)
    // Handle version
    lastVersion.mergeEdgeTypesInPlace(keep, change)
    lastVersion ++= diff
    assert(diff.nonEmpty)
    this
  }

  override def resetVersion(): this.type = {
    lastVersion = VocabularyHyperGraph.empty
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
