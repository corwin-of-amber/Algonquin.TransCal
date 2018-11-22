package structures

import structures.immutable.Item

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOneLike[Node, EdgeType, +This <: HyperGraphManyWithOrderToOneLike[Node, EdgeType, This]] {

  def findEdges(edge: EdgeType): Set[HyperEdge[Node, EdgeType]]

  def find[Id](pattern: (Item[Node, Id], Item[EdgeType, Id], Seq[Item[Node, Id]])): Set[HyperEdge[Node, EdgeType]]

  def findSubgraph[Id, Pattern <: HyperGraphManyWithOrderToOneLike[Item[Node, Id], Item[EdgeType, Id], Pattern]](hyperPattern: Pattern): Set[Map[Id, Either[Node, EdgeType]]]

  def cycles: Boolean

  def nodes: Set[Node] = edges.flatMap(edge=>edge.target +: edge.sources)

  def edgeTypes: Set[EdgeType] = edges.flatMap(edge=>Set(edge.edgeType))

  def edges: Set[HyperEdge[Node, EdgeType]]

  def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): This = {
    addEdge(hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources)
  }
  def addEdge(hyperEdge: (Node, EdgeType, Seq[Node])): This = {
    addEdge(hyperEdge._1, hyperEdge._2, hyperEdge._3)
  }

  def addEdge(target: Node, edgeType: EdgeType, sources: Seq[Node]): This

  def removeEdge(hyperEdge: (Node, EdgeType, Seq[Node])): This = {
    removeEdge(hyperEdge._1, hyperEdge._2, hyperEdge._3)
  }
  def removeEdge(target: Node, edge: EdgeType, sources: Seq[Node]): This

  def mergeNodes(keep: Node, change: Node): This
}