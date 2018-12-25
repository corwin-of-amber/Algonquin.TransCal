package structures

/**
  * @author tomer
  * @since 12/25/18
  */
case class HyperEdge[Node, EdgeType](target: Node, edgeType: EdgeType, sources:Seq[Node])
