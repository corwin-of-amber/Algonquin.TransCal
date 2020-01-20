package structures.mutable

import structures._
import structures.generic.{HyperGraphLikeGenericCompanion, VersionedHyperGraphLike}

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class  VersionedHyperGraph[Node, EdgeType] private(wrapped: VocabularyHyperGraph[Node, EdgeType], var lastVersion: VocabularyHyperGraph[Node, EdgeType], var locked: Boolean)
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped)
    with VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]] {

  /* --- Constructors --- */

  def this(wrapped: VocabularyHyperGraph[Node, EdgeType]) = {
    this(wrapped.clone, wrapped.clone, false)
  }

  /* --- Public Methods --- */

  override def clone = new VersionedHyperGraph[Node, EdgeType](wrapped.clone, lastVersion.clone, isLocked)

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
      if (!isLocked)
        lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
      lastVersion += hyperEdge
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
      if (!isLocked)
        lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
      lastVersion ++= newEdges
    }
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findByEdgeType(change).map(e => swapEdgeType(e, keep, change))
    // Handle wrapped
    wrapped.mergeEdgeTypesInPlace(keep, change)
    // Handle version
    if (!isLocked)
      lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
    lastVersion.mergeEdgeTypesInPlace(keep, change)
    lastVersion ++= diff
    this
  }

  override def mergeNodesInPlace(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findInNodes(change).map(e => swapNode(e, keep, change))
    // Handle wrapped
    wrapped.mergeNodesInPlace(keep, change)
    // Handle version
    if (!isLocked)
      lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
    lastVersion.mergeNodesInPlace(keep, change)
    lastVersion ++= diff
    this
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

  override def isLocked: Boolean = locked

  override def lockVersions(): VersionedHyperGraph[Node, EdgeType] = clone.lockVersionsInPlace()

  override def unlockVersions(): VersionedHyperGraph[Node, EdgeType] = clone.unlockVersionsInPlace()

  def lockVersionsInPlace(): VersionedHyperGraph[Node, EdgeType] = {
    locked = true;
    lastVersion = VocabularyHyperGraph.empty[Node, EdgeType]
    this
  }

  def unlockVersionsInPlace(): VersionedHyperGraph[Node, EdgeType] = {
    locked = false;
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
