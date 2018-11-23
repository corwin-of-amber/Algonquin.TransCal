package structures

import structures.immutable.Item

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOneLike[Node, EdgeType, +This <: HyperGraphManyWithOrderToOneLike[Node, EdgeType, This]] {

  def findEdges(edge: EdgeType): Set[HyperEdge[Node, EdgeType]]

  def find[Id](pattern: HyperEdge[Item[Node, Id], Item[EdgeType, Id]]): Set[HyperEdge[Node, EdgeType]]

  def findSubgraph[Id, Pattern <: HyperGraphManyWithOrderToOneLike[Item[Node, Id], Item[EdgeType, Id], Pattern]](hyperPattern: Pattern): Set[Map[Id, Either[Node, EdgeType]]]

  def cycles: Boolean

  def nodes: Set[Node] = edges.flatMap(edge=>edge.target +: edge.sources)

  def edgeTypes: Set[EdgeType] = edges.flatMap(edge=>Set(edge.edgeType))

  def edges: Set[HyperEdge[Node, EdgeType]]

  def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): This

  def removeEdge(hyperEdge: HyperEdge[Node, EdgeType]): This

  def mergeNodes(keep: Node, change: Node): This
}