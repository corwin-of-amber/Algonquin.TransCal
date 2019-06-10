package structures.immutable

import structures.HyperGraphManyWithOrderToOneLike._
import structures._

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  * @author tomer
  * @since 11/15/18
  */
abstract class WrapperHyperGraph[Node, EdgeType, +This <: WrapperHyperGraph[Node, EdgeType, This]] protected (wrapped: HyperGraphManyWithOrderToOne[Node, EdgeType])
  extends HyperGraphManyWithOrderToOne[Node, EdgeType]
    with HyperGraphManyWithOrderToOneLike[Node, EdgeType, This] {

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): This = newBuilder.++=(wrapped.addEdge(hyperEdge)).result()

  override def addEdges(hyperEdges: Set[HyperEdge[Node, EdgeType]]): This = newBuilder.++=(wrapped.addEdges(hyperEdges)).result()

  override def removeEdge(hyperEdge: HyperEdge[Node, EdgeType]): This = newBuilder.++=(wrapped.removeEdge(hyperEdge)).result()

  override def mergeNodes(keep: Node, change: Node): This = newBuilder.++=(wrapped.mergeNodes(keep, change)).result()

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): This = newBuilder.++=(wrapped.mergeEdgeTypes(keep, change)).result()

  override def findRegex[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(HyperEdge[Node, EdgeType], Map[Id, Node], Map[Id, EdgeType])] = wrapped.findRegex(pattern)

  override def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern]](hyperPattern: Pattern): Set[(Map[Id, Node], Map[Id, EdgeType])] = wrapped.findSubgraph(hyperPattern)

  override def edges: Set[HyperEdge[Node, EdgeType]] = wrapped.edges

  /* --- Object Impl. --- */

  override def hashCode(): Int = wrapped.hashCode

  override def equals(obj: Any): Boolean = wrapped.equals(obj)
}


