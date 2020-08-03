package structures.generic

import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}
import structures.{HyperEdge, HyperGraph, HyperGraphLike, Metadata}

abstract class WrapperHyperGraph[Node, EdgeType, +This <: WrapperHyperGraph[Node, EdgeType, This]] protected(wrapped: HyperGraph[Node, EdgeType])
  extends HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, This] {

  override def empty: This = newBuilder.result()

  override def edges: Set[HyperEdge[Node, EdgeType]] = wrapped.edges

  override def nodes: Set[Node] = wrapped.nodes

  override def targets: Set[Node] = wrapped.targets

  override def edgeTypes: Set[EdgeType] = wrapped.edgeTypes

  override def size: Int = wrapped.size

  override def findRegex[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[structures.Match[Node, EdgeType, Id]] = wrapped.findRegex(pattern)

  override def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with collection.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern): Set[structures.Match[Node, EdgeType, Id]] = wrapped.findSubgraph(hyperPattern)

  override def findInSources(n: Node): Set[HyperEdge[Node, EdgeType]] = wrapped.findInSources(n)

  override def findByTarget(n: Node): Set[HyperEdge[Node, EdgeType]] = wrapped.findByTarget(n)

  override def findByEdgeType(et: EdgeType): Set[HyperEdge[Node, EdgeType]] = wrapped.findByEdgeType(et)

  /* --- Object Impl. --- */

  override def hashCode(): Int = wrapped.hashCode()

  override def equals(obj: Any): Boolean = wrapped.equals(obj)

  override def contains(elem: HyperEdge[Node, EdgeType]): Boolean = wrapped.contains(elem)

  override def updateMetadata(edge: HyperEdge[Node, EdgeType], metadata: Metadata): Unit = wrapped.updateMetadata(edge, metadata)

  override def markNode(n: Node): HyperGraphLike.Marker[Node] = wrapped.markNode(n)

  override def markEdgeType(e: EdgeType): HyperGraphLike.Marker[EdgeType] = wrapped.markEdgeType(e)

  override def getMarkedNode(m: HyperGraphLike.Marker[Node]): Node = wrapped.getMarkedNode(m)

  override def getMarkedEdgeType(m: HyperGraphLike.Marker[EdgeType]): EdgeType = wrapped.getMarkedEdgeType(m)

  override def getMarkedHyperEdge(m: HyperGraphLike.Marker[HyperEdge[Node, EdgeType]]): HyperEdge[Node, EdgeType] = wrapped.getMarkedHyperEdge(m)
}