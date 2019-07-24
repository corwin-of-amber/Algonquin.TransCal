package structures.mutable


import structures.HyperEdge

import scala.collection.GenTraversableOnce


/** A hyper graph from many to one.
  *
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphLike[Node, EdgeType, +This <: HyperGraphLike[Node, EdgeType, This] with Set[HyperEdge[Node, EdgeType]]]
  extends structures.HyperGraphLike[Node, EdgeType, This] {
  def +=(hyperEdge: HyperEdge[Node, EdgeType]): Unit

  def ++=(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): Unit

  def -=(hyperEdge: HyperEdge[Node, EdgeType]): Unit
}