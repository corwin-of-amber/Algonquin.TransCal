package structures.mutable

import structures.HyperEdge
import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  * @author tomer
  * @since 11/15/18
  */
abstract class WrapperHyperGraph[Node, EdgeType, +This <: WrapperHyperGraph[Node, EdgeType, This]] protected (wrapped: HyperGraph[Node, EdgeType])
  extends HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, This] {

  override def empty: This = newBuilder.result()

  /* --- HyperGraphManyWithOrderToOne Impl. --- */


  override def -=(elem: HyperEdge[Node, EdgeType]): this.type = {wrapped -= elem; this }

  override def mergeNodes(keep: Node, change: Node): This = newBuilder.++=(wrapped.mergeNodes(keep, change)).result()

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): This = newBuilder.++=(wrapped.mergeEdgeTypes(keep, change)).result()

  override def findRegex[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(HyperEdge[Node, EdgeType], Map[Id, Node], Map[Id, EdgeType])] = wrapped.findRegex(pattern)

  override def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with collection.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern): Set[(Map[Id, Node], Map[Id, EdgeType])] = wrapped.findSubgraph(hyperPattern)

  override def edges: Set[HyperEdge[Node, EdgeType]] = wrapped.edges

  /* --- Object Impl. --- */

  override def hashCode(): Int = wrapped.hashCode()

  override def equals(obj: Any): Boolean = wrapped.equals(obj)
}


