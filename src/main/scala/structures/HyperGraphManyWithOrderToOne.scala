package structures

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType] extends HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]]

object HyperGraphManyWithOrderToOne {

  // Reference HyperGraphManyWithOrderToOneLike
  type HyperEdge[Node, EdgeType] = HyperGraphManyWithOrderToOneLike.HyperEdge[Node, EdgeType]
  val HyperEdge: HyperGraphManyWithOrderToOneLike.HyperEdge.type = HyperGraphManyWithOrderToOneLike.HyperEdge
  type Item[Value, Id] = HyperGraphManyWithOrderToOneLike.Item[Value, Id]
  type Hole[Value, Id] = HyperGraphManyWithOrderToOneLike.Hole[Value, Id]
  val Hole: HyperGraphManyWithOrderToOneLike.Hole.type = HyperGraphManyWithOrderToOneLike.Hole
  type Explicit[Value, Id] = HyperGraphManyWithOrderToOneLike.Explicit[Value, Id]
  val Explicit: HyperGraphManyWithOrderToOneLike.Explicit.type = HyperGraphManyWithOrderToOneLike.Explicit
  type Ignored[Value, Id] = HyperGraphManyWithOrderToOneLike.Ignored[Value, Id]
  val Ignored: HyperGraphManyWithOrderToOneLike.Ignored.type = HyperGraphManyWithOrderToOneLike.Ignored
  type HyperEdgePattern[Node, EdgeType, Id] = HyperGraphManyWithOrderToOneLike.HyperEdgePattern[Node, EdgeType, Id]
  type HyperGraphPattern[Node, EdgeType, Id, +This <: HyperGraphPattern[Node, EdgeType, Id, This]] = HyperGraphManyWithOrderToOneLike.HyperGraphPattern[Node, EdgeType, Id, This]
}
