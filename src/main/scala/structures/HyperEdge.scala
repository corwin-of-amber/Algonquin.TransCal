package structures

/**
  * @author tomer
  * @since 11/22/18
  */
case class HyperEdge[Node, EdgeType](target: Node, edgeType: EdgeType, sources:Seq[Node])
