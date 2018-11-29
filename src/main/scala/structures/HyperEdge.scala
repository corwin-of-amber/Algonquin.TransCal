package structures

/**
  * @author tomer
  * @since 11/22/18
  */
class HyperEdge[Node, EdgeType](val target: Node, val edgeType: EdgeType, val sources:Seq[Node]) {
  override def toString: String = f"HyperEdge($target, $edgeType, $sources)"
}
object HyperEdge {
  def apply[Node, EdgeType] (target: Node, edgeType: EdgeType, sources: Seq[Node]): HyperEdge[Node, EdgeType] = new HyperEdge(target, edgeType, sources)
  def unapply[Node, EdgeType](hyperEdge: HyperEdge[Node, EdgeType]): Option[(Node, EdgeType, Seq[Node])] = Some(hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources)
}
