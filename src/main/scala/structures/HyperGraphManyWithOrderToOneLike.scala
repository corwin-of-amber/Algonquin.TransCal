package structures

import structures.immutable.Item

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOneLike[Node, EdgeType, +This <: HyperGraphManyWithOrderToOneLike[Node, EdgeType, This]] {

  def findEdges(edge: EdgeType): Set[HyperGraphManyWithOrderToOneLike.HyperEdge[Node, EdgeType]]

  def find[Id](pattern: (Item[Node, Id], Item[EdgeType, Id], Seq[Item[Node, Id]])): Set[HyperGraphManyWithOrderToOneLike.HyperEdge[Node, EdgeType]]

  def cycles: Boolean

  def nodes: Set[Node]

  def edges: Set[EdgeType]

  def addEdge(hyperEdge: HyperGraphManyWithOrderToOneLike.HyperEdge[Node, EdgeType]): This = {
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

object HyperGraphManyWithOrderToOneLike {
  class HyperEdge[Node, EdgeType](val target: Node, val edgeType: EdgeType, val sources:Seq[Node])
  object HyperEdge {
    def apply[Node, EdgeType] (target: Node, edgeType: EdgeType, sources: Seq[Node]): HyperEdge[Node, EdgeType] = new HyperEdge(target, edgeType, sources)
    def unapply[Node, EdgeType](hyperEdge: HyperEdge[Node, EdgeType]): Option[(Node, EdgeType, Seq[Node])] = Some(hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources)
  }
}