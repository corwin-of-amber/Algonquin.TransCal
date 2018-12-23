package structures

import structures.HyperGraphManyWithOrderToOneLike.{HyperEdge, HyperEdgePattern, HyperGraphPattern}

/** A hyper graph from many to one.
  *
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOneLike[Node, EdgeType, +This <: HyperGraphManyWithOrderToOneLike[Node, EdgeType, This]] {

  /** Finds all the edges with the EdgeType
    *
    * @param edgeType to search.
    * @return correspond edges.
    */
  def findEdges(edgeType: EdgeType): Set[HyperEdge[Node, EdgeType]]

  /**Find a pattern of an edge in the graph.
    *
    * @param pattern The pattern of an edge.
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @return The matched edges
    */
  def find[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[HyperEdge[Node, EdgeType]]

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @tparam Pattern The type of the pattern subgraph
    * @return The matched references.
    */
  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern]](hyperPattern: Pattern): Set[Map[Id, Either[Node, EdgeType]]]

  /**
    * @return all the nodes in the hyper graph.
    */
  def nodes: Set[Node] = edges.flatMap(edge=>edge.target +: edge.sources)

  /**
    * @return all the edge types in the hyper graph.
    */
  def edgeTypes: Set[EdgeType] = edges.flatMap(edge=>Set(edge.edgeType))

  /**
    * @return all the edges in the hyper graph.
    */
  def edges: Set[HyperEdge[Node, EdgeType]]

  /** Adds an edge to the hyper graph.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): This

  /** Adds edges to the hyper graph.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edges.
    */
  def addEdges(hyperEdges: Set[HyperEdge[Node, EdgeType]]): This

  /** Removes an edge from the hyper graph.
    *
    * @param hyperEdge The edge to remove.
    * @return The new hyper graph without the edge.
    */
  def removeEdge(hyperEdge: HyperEdge[Node, EdgeType]): This

  /** Merges two node to one.
    *
    * @param keep The node to change to.
    * @param change The node to change from.
    * @return The new graph after the change.
    */
  def mergeNodes(keep: Node, change: Node): This

  /** Merges two edge types to one.
    *
    * @param keep The edge to change to.
    * @param change The edge to change from.
    * @return The new graph after the change.
    */
  def mergeEdgeTypes(keep: EdgeType, change: EdgeType): This

  /** Build a new hyper graph with the other hyper graph.
    *
    * @param hyperGraph Other hyper graph.
    * @tparam Other Type of hyper graph
    * @return New hyper graph includes the other.
    */
  def ++[Other <: HyperGraphManyWithOrderToOneLike[Node, EdgeType, Other]](hyperGraph: Other): This
}

object HyperGraphManyWithOrderToOneLike {


  /* --- Public --- */

  case class HyperEdge[Node, EdgeType](target: Node, edgeType: EdgeType, sources:Seq[Node])

  // Reference VocabularyLike.Item from HyperGraphManyWithOrderToOneLike
  type Item[Value, Id] = VocabularyLike.Item[Value, Id]
  type Hole[Value, Id] = VocabularyLike.Hole[Value, Id]
  val Hole: VocabularyLike.Hole.type = VocabularyLike.Hole
  type Explicit[Value, Id] = VocabularyLike.Explicit[Value, Id]
  val Explicit: VocabularyLike.Explicit.type = VocabularyLike.Explicit
  type Ignored[Value, Id] = VocabularyLike.Ignored[Value, Id]
  val Ignored: VocabularyLike.Ignored.type = VocabularyLike.Ignored

  // Shortcuts
  type HyperEdgePattern[Node, EdgeType, Id] = HyperEdge[Item[Node, Id], Item[EdgeType, Id]]
  type HyperGraphPattern[Node, EdgeType, Id, +This <: HyperGraphPattern[Node, EdgeType, Id, This]] = HyperGraphManyWithOrderToOneLike[Item[Node, Id], Item[EdgeType, Id], This]
}