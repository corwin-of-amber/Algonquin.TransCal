package structures.mutable

import structures.HyperEdge
import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  * @author tomer
  * @since 11/15/18
  */
abstract class WrapperHyperGraph[Node, EdgeType, +This <: WrapperHyperGraph[Node, EdgeType, This]] protected (wrapped: HyperGraph[Node, EdgeType])
  extends structures.generic.WrapperHyperGraph[Node, EdgeType, This](wrapped) with HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, This] {

  override def empty: This = newBuilder.result()

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def -=(elem: HyperEdge[Node, EdgeType]): this.type = {wrapped -= elem; this }
}


