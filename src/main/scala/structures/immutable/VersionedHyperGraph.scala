package structures.immutable

import structures._
import structures.generic.{HyperGraphLikeGenericCompanion, VersionedHyperGraphLike}

import scala.collection.{GenTraversableOnce, mutable}

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: VocabularyHyperGraph[Node, EdgeType], lastVersion: HyperGraph[Node, EdgeType])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped)
    with VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]] {

  /* --- Constructors --- */

  def this(wrapped: VocabularyHyperGraph[Node, EdgeType]) = {
    this(wrapped, wrapped)
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
    else new VersionedHyperGraph(wrapped.+(hyperEdge), VocabularyHyperGraph.empty + hyperEdge)
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
    else new VersionedHyperGraph(wrapped.++(newEdges), VocabularyHyperGraph.empty ++ hyperEdges)
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findByEdgeType(change).map(e => swapEdgeType(e, keep, change))
    // Handle wrapped
    val newWrapped = wrapped.mergeEdgeTypes(keep, change)
    // Handle version
    new VersionedHyperGraph(newWrapped, VocabularyHyperGraph.empty ++ diff)
  }

  override def mergeNodes(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findInNodes(change).map(e => swapNode(e, keep, change))
    // Handle wrapped
    val newWrapped = wrapped.mergeNodes(keep, change)
    // Handle version
    new VersionedHyperGraph(newWrapped, VocabularyHyperGraph.empty ++ diff)
  }

  /* --- Object Impl. --- */

  override lazy val toString: String = f"VersionedHyperGraph($wrapped, $lastVersion)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        val innerHyperGraph = VocabularyHyperGraph(parts: _*)
        new VersionedHyperGraph(innerHyperGraph, innerHyperGraph)
      }
    }

  override protected def getHyperGraph: generic.HyperGraph[Node, EdgeType] = wrapped

  override protected def getLastVersion: generic.HyperGraph[Node, EdgeType] = lastVersion

  override def addKeepVersion(hyperEdge: HyperEdge[Node, EdgeType]): VersionedHyperGraph[Node, EdgeType] =
    if (contains(hyperEdge)) this
    else new VersionedHyperGraph(wrapped.+(hyperEdge), lastVersion + hyperEdge)

  override def addAllKeepVersion(hyperEdges: Set[HyperEdge[Node, EdgeType]]): VersionedHyperGraph[Node, EdgeType] = {
    val newEdges = hyperEdges.toSeq.filterNot(contains)
    if (newEdges.isEmpty) this
    else new VersionedHyperGraph(wrapped.++(newEdges), VocabularyHyperGraph.empty ++ hyperEdges)
  }

  override def mergeNodesKeepVersion(keep: Node, change: Node): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findInNodes(change).map(e => swapNode(e, keep, change))
    // Handle wrapped
    val newWrapped = wrapped.mergeNodes(keep, change)
    // Handle version
    new VersionedHyperGraph(newWrapped, lastVersion ++ diff)
  }

  override def mergeEdgeTypesKeepVersion(keep: EdgeType, change: EdgeType): VersionedHyperGraph[Node, EdgeType] = {
    val diff = findByEdgeType(change).map(e => swapEdgeType(e, keep, change))
    // Handle wrapped
    val newWrapped = wrapped.mergeEdgeTypes(keep, change)
    // Handle version
    new VersionedHyperGraph(newWrapped, lastVersion ++ diff)
  }

  override def resetVersion(): VersionedHyperGraph[Node, EdgeType] = new VersionedHyperGraph(wrapped, VocabularyHyperGraph.empty[Node, EdgeType])
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
