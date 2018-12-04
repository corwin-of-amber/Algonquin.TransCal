package structures

import structures.HyperGraphManyWithOrderToOneLike.{HyperEdge, Item}

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
  def find[Id](pattern: HyperEdge[Item[Node, Id], Item[EdgeType, Id]]): Set[HyperEdge[Node, EdgeType]]

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @tparam Pattern The type of the pattern subgraph
    * @return The matched references.
    */
  def findSubgraph[Id, Pattern <: HyperGraphManyWithOrderToOneLike[Item[Node, Id], Item[EdgeType, Id], Pattern]](hyperPattern: Pattern): Set[Map[Id, Either[Node, EdgeType]]]

  /** Checks if there are any cycles in the hyper graph.
    *
    * @return True is there is a cycle and false otherwise.
    */
  def cycles: Boolean

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

  /** Removes an edge from the hyper graph.
    *
    * @param hyperEdge The edge to remove.
    * @return The new hyper graph without the edge.
    */
  def removeEdge(hyperEdge: HyperEdge[Node, EdgeType]): This

  /** Merges two node to one.
    *
    * @param keep The edge to change to.
    * @param change The edge to change from.
    * @return The new graph after the change.
    */
  def mergeNodes(keep: Node, change: Node): This
}

object HyperGraphManyWithOrderToOneLike {
  case class HyperEdge[Node, EdgeType](target: Node, edgeType: EdgeType, sources:Seq[Node])

  trait Item[Value, Id]
  case class Hole[Value, Id](id: Id) extends Item[Value, Id]
  case class Explicit[Value, Id](value: Value) extends Item[Value, Id]
  case class Ignored[Value, Id]() extends Item[Value, Id]
}