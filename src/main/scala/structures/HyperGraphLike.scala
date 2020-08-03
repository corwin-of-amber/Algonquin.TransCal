package structures

import structures.HyperGraphLike.{HyperEdgeMarker, HyperEdgePattern, HyperGraphPattern, Marker}

import scala.collection.{GenTraversableOnce, SetLike}

/** A hyper graph from many to one.
  *
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphLike[Node, EdgeType, +This <: HyperGraphLike[Node, EdgeType, This] with collection.Set[HyperEdge[Node, EdgeType]]]
  extends SetLike[HyperEdge[Node, EdgeType], This] {

  /** Find a pattern of an edge in the graph.
    *
    * @param pattern The pattern of an edge.
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @return The matched edges
    */
  def findRegex[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[Match[Node, EdgeType, Id]]
  def findRegexHyperEdges[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[HyperEdge[Node, EdgeType]] = findRegex(pattern).flatMap(_.edges)
  def findRegexMaps[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(Map[Id, Node], Map[Id, EdgeType])] = findRegex(pattern).map(t => (t.nodeMap, t.edgeMap))
  def contains(elem: HyperEdge[Node, EdgeType]): Boolean = findRegex(HyperEdge(Explicit(elem.target), Explicit(elem.edgeType), elem.sources.map(Explicit(_)), elem.metadata)).nonEmpty

  def findInSources(n: Node): Set[HyperEdge[Node, EdgeType]]
  def findByTarget(n: Node): Set[HyperEdge[Node, EdgeType]]
  def findInNodes(n: Node): Set[HyperEdge[Node, EdgeType]] = findByTarget(n) ++ findInSources(n)
  def findByEdgeType(et: EdgeType): Set[HyperEdge[Node, EdgeType]]

  def markNode(n: Node): Marker[Node]

  def markEdgeType(e: EdgeType): Marker[EdgeType]

  def markHyperEdge(e: HyperEdge[Node, EdgeType]): Marker[HyperEdge[Node, EdgeType]] =
    HyperEdgeMarker[Node, EdgeType](markEdgeType(e.edgeType), markNode(e.target), e.sources.map(n => markNode(n)))

  def getMarkedNode(m: Marker[Node]): Node

  def getMarkedEdgeType(m: Marker[EdgeType]): EdgeType

  def getMarkedHyperEdge(m: Marker[HyperEdge[Node, EdgeType]]): HyperEdge[Node, EdgeType]

  /** Finds all the edges with the EdgeType
    *
    * @param edgeType to search.
    * @return correspond edges.
    */
  def findEdges(edgeType: EdgeType): Set[HyperEdge[Node, EdgeType]] = findByEdgeType(edgeType)

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id      A reference type to show a wanted connection in the pattern.
    * @tparam Pattern The type of the pattern subgraph
    * @return The matched references.
    */
  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with collection.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern): Set[Match[Node, EdgeType, Id]]

  /**
    * @return all the nodes in the hyper graph.
    */
//  def nodes: Set[Node] = edges.flatMap(edge => edge.target +: edge.sources)
  def nodes: Set[Node] = edges.flatMap(e => e.target +: e.sources)

  /**
    * @return all the nodes that apear as a target in the hyper graph.
    */
  def targets: Set[Node] = edges.map(_.target)

  /**
    * @return all the edge types in the hyper graph.
    */
  def edgeTypes: Set[EdgeType] = edges.map(_.edgeType)

  /**
    * @return all the edges in the hyper graph.
    */
  def edges: Set[HyperEdge[Node, EdgeType]]

  /**
    * @return all the edges in the hyper graph ordered by target.
    */
  def edgesOrdered(implicit ordering: Ordering[HyperEdge[Node, EdgeType]]): Seq[HyperEdge[Node, EdgeType]] = edges.toSeq.sorted(ordering)

  /** Adds an edge to the hyper graph.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  def +(hyperEdge: HyperEdge[Node, EdgeType]): This

  /** Adds edges to the hyper graph.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edges.
    */
  def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): This

  /** Removes an edge from the hyper graph.
    *
    * @param hyperEdge The edge to remove.
    * @return The new hyper graph without the edge.
    */
  def -(hyperEdge: HyperEdge[Node, EdgeType]): This

  /** Merges two node to one.
    *
    * @param keep   The node to change to.
    * @param change The node to change from.
    * @return The new graph after the change.
    */
  def mergeNodes(keep: Node, change: Node): This

  /** Merges two edge types to one.
    *
    * @param keep   The edge to change to.
    * @param change The edge to change from.
    * @return The new graph after the change.
    */
  def mergeEdgeTypes(keep: EdgeType, change: EdgeType): This

  def updateMetadata(edge: HyperEdge[Node, EdgeType], metadata: Metadata): Unit

//  /** Create a new builder from current data. When adding an edge to builder it should update the metadatastructure and
//    * update the future vocabulary result.
//    *
//    * @return new builder for current state of graph.
//    */
//  def copyBuilder: collection.mutable.Builder[HyperEdge[Node, EdgeType], This] with Shrinkable[HyperEdge[Node, EdgeType]]

  /* --- IterableLike Impl. --- */

  override def iterator: Iterator[HyperEdge[Node, EdgeType]] = edges.iterator

  /* --- Object Impl. --- */

  override def hashCode(): Int = edges.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case x: HyperGraphLike[Node, EdgeType, This] => x.edges == edges
    case _ => false
  }
}


object HyperGraphLike {
  // Shortcuts
  type HyperEdgePattern[Node, EdgeType, Id] = HyperEdge[Item[Node, Id], Item[EdgeType, Id]]
  type HyperGraphPattern[Node, EdgeType, Id, +This <: HyperGraphPattern[Node, EdgeType, Id, This] with collection.Set[HyperEdgePattern[Node, EdgeType, Id]]] = HyperGraphLike[Item[Node, Id], Item[EdgeType, Id], This]

  class Marker[T]

//  case class EitherMarker[L, R](either: Either[Marker[L], Marker[R]]) extends Marker[Either[L, R]]
//
//  case class SequenceMarker[T](s: Seq[Marker[T]]) extends Marker[Seq[T]]

  case class HyperEdgeMarker[N, ET](et: Marker[ET], target: Marker[N], sources: Seq[Marker[N]]) extends Marker[HyperEdge[N, ET]]

}