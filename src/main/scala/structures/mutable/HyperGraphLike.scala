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
}