package structures.immutable

import structures.HyperGraphManyWithOrderToOneLike

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType] extends structures.HyperGraphManyWithOrderToOne[Node, EdgeType]
  with HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]]

object HyperGraphManyWithOrderToOne {
  def empty[Node, EdgeType]: HyperGraphManyWithOrderToOne[Node, EdgeType] = VocabularyHyperGraph.empty

  def apply[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): HyperGraphManyWithOrderToOne[Node, EdgeType] = VocabularyHyperGraph(edges)

  // Reference structures.HyperGraphManyWithOrderToOne
  type HyperEdge[Node, EdgeType] = structures.HyperGraphManyWithOrderToOne.HyperEdge[Node, EdgeType]
  val HyperEdge: structures.HyperGraphManyWithOrderToOne.HyperEdge.type = structures.HyperGraphManyWithOrderToOne.HyperEdge
  type Item[Value, Id] = structures.HyperGraphManyWithOrderToOne.Item[Value, Id]
  type Hole[Value, Id] = structures.HyperGraphManyWithOrderToOne.Hole[Value, Id]
  val Hole: structures.HyperGraphManyWithOrderToOne.Hole.type = structures.HyperGraphManyWithOrderToOne.Hole
  type Explicit[Value, Id] = structures.HyperGraphManyWithOrderToOne.Explicit[Value, Id]
  val Explicit: structures.HyperGraphManyWithOrderToOne.Explicit.type = structures.HyperGraphManyWithOrderToOne.Explicit
  type Ignored[Value, Id] = structures.HyperGraphManyWithOrderToOne.Ignored[Value, Id]
  val Ignored: structures.HyperGraphManyWithOrderToOne.Ignored.type = structures.HyperGraphManyWithOrderToOne.Ignored
  type HyperEdgePattern[Node, EdgeType, Id] = structures.HyperGraphManyWithOrderToOne.HyperEdgePattern[Node, EdgeType, Id]
  type HyperGraphPattern[Node, EdgeType, Id, +This <: HyperGraphPattern[Node, EdgeType, Id, This]] = structures.HyperGraphManyWithOrderToOne.HyperGraphPattern[Node, EdgeType, Id, This]
}
