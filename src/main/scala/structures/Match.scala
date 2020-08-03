package structures

case class Match[Node, EdgeType, Id](edges: Set[HyperEdge[Node, EdgeType]], nodeMap: Map[Id, Node], edgeMap: Map[Id, EdgeType]) {
  def merge(other: Match[Node, EdgeType, Id]): Match[Node, EdgeType, Id] = Match(edges ++ other.edges, nodeMap ++ other.nodeMap, edgeMap ++ other.edgeMap)
}

object Match {
  def empty[Node, EdgeType, Id]: Match[Node, EdgeType, Id] = Match(Set.empty, Map.empty, Map.empty)
}