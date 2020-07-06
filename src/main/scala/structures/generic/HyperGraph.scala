package structures.generic

import structures._

trait HyperGraph[Node, EdgeType]
  extends collection.Set[HyperEdge[Node, EdgeType]]
    with HyperGraphLike[Node, EdgeType, HyperGraph[Node, EdgeType]] {

  override def empty: HyperGraph[Node, EdgeType] = HyperGraph.empty

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @return The matched references.
    */
  def findSubgraph[Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]]): Set[HyperGraph.Match[Node, EdgeType, Id]] =
    findSubgraph[Id, HyperGraph[Item[Node, Id], Item[EdgeType, Id]]](hyperPattern)
}

object HyperGraph extends HyperGraphLikeGenericCompanion[HyperGraph] {

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: collection.mutable.Builder[HyperEdge[A, B], HyperGraph[A, B]] =
    structures.mutable.HyperGraph.newBuilder

  import play.api.libs.json._
  case class JsonGraph(edges: Seq[HyperEdge.JsonEdge])
  val graphFormat: OFormat[JsonGraph] = {
    implicit val edgeFormat: OFormat[HyperEdge.JsonEdge] = HyperEdge.edgeFormat
    Json.format[JsonGraph]
  }

  case class Match[Node, EdgeType, Id](edges: Set[HyperEdge[Node, EdgeType]], nodeMap: Map[Id, Node], edgeMap: Map[Id, EdgeType]) {
    def merge(other: Match[Node, EdgeType, Id]): Match[Node, EdgeType, Id] = Match(edges ++ other.edges, nodeMap ++ other.nodeMap, edgeMap ++ other.edgeMap)
  }

  object Match {
    def empty[Node, EdgeType, Id]: Match[Node, EdgeType, Id] = Match(Set.empty, Map.empty, Map.empty)
  }
}
