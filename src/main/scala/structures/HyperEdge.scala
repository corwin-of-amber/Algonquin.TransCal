package structures

/**
  * @author tomer
  * @since 11/22/18
  */
case class HyperEdge[Node, EdgeType](val target: Node, val edgeType: EdgeType, val sources:Seq[Node])
