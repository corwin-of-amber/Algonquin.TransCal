package structures

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
  def findSubgraph[Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]]): Set[Match[Node, EdgeType, Id]] =
    findSubgraph[Id, HyperGraph[Item[Node, Id], Item[EdgeType, Id]]](hyperPattern)
}

object HyperGraph extends HyperGraphCompanion[HyperGraph] {

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
}
