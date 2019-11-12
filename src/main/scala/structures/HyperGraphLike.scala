package structures

import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}

import scala.collection.generic.Shrinkable
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
  def findRegex[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(HyperEdge[Node, EdgeType], Map[Id, Node], Map[Id, EdgeType])]
  def findRegexHyperEdges[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[HyperEdge[Node, EdgeType]] = findRegex(pattern).map(_._1)
  def findRegexMaps[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(Map[Id, Node], Map[Id, EdgeType])] = findRegex(pattern).map(t => (t._2, t._3))
  def contains(elem: HyperEdge[Node, EdgeType]): Boolean = findRegex(HyperEdge(Explicit(elem.target), Explicit(elem.edgeType), elem.sources.map(Explicit(_)), elem.metadata)).nonEmpty

  def findInSources[Id](n: Node): Set[HyperEdge[Node, EdgeType]] = findRegexHyperEdges(HyperEdge(Ignored(), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()), Explicit(n), Repetition.rep0(Int.MaxValue, Ignored())), EmptyMetadata))
  def findByTarget[Id](n: Node): Set[HyperEdge[Node, EdgeType]] = findRegexHyperEdges(HyperEdge(Explicit(n), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored())), EmptyMetadata))
  def findInNodes[Id](n: Node): Set[HyperEdge[Node, EdgeType]] = findByTarget(n) ++ findInSources(n)
  def findByEdgeType[Id](et: EdgeType): Set[HyperEdge[Node, EdgeType]] = findRegexHyperEdges(HyperEdge(Ignored(), Explicit(et), Seq(Repetition.rep0(Int.MaxValue, Ignored())), EmptyMetadata))


  /** Finds all the edges with the EdgeType
    *
    * @param edgeType to search.
    * @return correspond edges.
    */
  def findEdges(edgeType: EdgeType): Set[HyperEdge[Node, EdgeType]] = findByEdgeType[Int](edgeType)

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id      A reference type to show a wanted connection in the pattern.
    * @tparam Pattern The type of the pattern subgraph
    * @return The matched references.
    */
  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with collection.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern): Set[(Map[Id, Node], Map[Id, EdgeType])]

  /**
    * @return all the nodes in the hyper graph.
    */
//  def nodes: Set[Node] = edges.flatMap(edge => edge.target +: edge.sources)
  def nodes: Set[Node] = findRegex[Int](HyperEdge(Hole(0), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Stream.from(1).map(Hole[Node, Int]))), EmptyMetadata)).flatMap(_._2.values.toSet)

  /**
    * @return all the nodes that apear as a target in the hyper graph.
    */
  def targets: Set[Node] = findRegex[Int](HyperEdge(Hole(0), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored())), EmptyMetadata)).flatMap(_._2.values.toSet)

  /**
    * @return all the edge types in the hyper graph.
    */
  def edgeTypes: Set[EdgeType] = findRegex(HyperEdge(Ignored(), Hole(0), Seq(Repetition.rep0(Int.MaxValue, Ignored())), EmptyMetadata)).flatMap(_._3.values.toSet)

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
}