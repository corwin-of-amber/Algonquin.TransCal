package structures.mutable


import structures.HyperEdge

import scala.collection.mutable


/** A hyper graph from many to one.
  *
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphLike[Node, EdgeType, +This <: HyperGraphLike[Node, EdgeType, This] with mutable.Set[HyperEdge[Node, EdgeType]]]
  extends structures.HyperGraphLike[Node, EdgeType, This]
     with mutable.SetLike[HyperEdge[Node, EdgeType], This] {

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): This = {
    clone().mergeEdgeTypesInPlace(keep, change)
  }

  override def mergeNodes(keep: Node, change: Node): This = {
    clone().mergeNodesInPlace(keep, change)
  }

  def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): This

  def mergeNodesInPlace(keep: Node, change: Node): This
}