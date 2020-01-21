package structures.immutable

import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}
import structures.{HyperEdge, HyperGraphLike}

import scala.collection.GenTraversableOnce

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  * @author tomer
  * @since 11/15/18
  */
abstract class WrapperHyperGraph[Node, EdgeType, +This <: WrapperHyperGraph[Node, EdgeType, This]] protected (wrapped: HyperGraph[Node, EdgeType])
  extends structures.generic.WrapperHyperGraph[Node, EdgeType, This](wrapped) with HyperGraph[Node, EdgeType] {

  override def empty: This = newBuilder.result()
  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def +(hyperEdge: HyperEdge[Node, EdgeType]): This = newBuilder.++=(wrapped.+(hyperEdge)).result()

  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): This = newBuilder.++=(wrapped.++(hyperEdges)).result()

  override def -(hyperEdge: HyperEdge[Node, EdgeType]): This = newBuilder.++=(wrapped.-(hyperEdge)).result()

  override def mergeNodes(keep: Node, change: Node): This = newBuilder.++=(wrapped.mergeNodes(keep, change)).result()

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): This = newBuilder.++=(wrapped.mergeEdgeTypes(keep, change)).result()
}


