package structures

import structures.immutable.Item

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOneLike[Node, Edge, +This <: HyperGraphManyWithOrderToOneLike[Node, Edge, This]] {

  def findEdges(edge: Edge): Set[(Node, Edge, Seq[Node])]

  def find[Id](pattern: (Item[Node, Id], Item[Edge, Id], Seq[Item[Node, Id]])): Set[(Node, Edge, Seq[Node])]

  def cycles: Boolean

  def nodes: Set[Node]

  def edges: Set[Edge]

  def addEdge(hyperEdge: (Node, Edge, Seq[Node])): This = {
    addEdge(hyperEdge._1, hyperEdge._2, hyperEdge._3)
  }

  def addEdge(target: Node, edge: Edge, sources: Seq[Node]): This

  def removeEdge(hyperEdge: (Node, Edge, Seq[Node])): This = {
    removeEdge(hyperEdge._1, hyperEdge._2, hyperEdge._3)
  }
  def removeEdge(target: Node, edge: Edge, sources: Seq[Node]): This

  def mergeNodes(keep: Node, change: Node): This
}
