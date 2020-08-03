package structures.mutable

import structures._
import structures.HyperGraph.{HyperGraphPattern, JsonGraph}
import synthesis.{HyperEdgeTargetOrdering, HyperTermId, HyperTermIdentifier}
import transcallang.Language

import scala.collection.mutable

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  *
  * @author tomer
  * @since 11/15/18
  */
class CompactHyperGraph[Node, EdgeType] private(wrapped: VersionedHyperGraph[Node, EdgeType])
  extends WrapperHyperGraph[Node, EdgeType, CompactHyperGraph[Node, EdgeType]](wrapped) {

  def this(edges: Set[HyperEdge[Node, EdgeType]]) = {
    this(VersionedHyperGraph.empty[Node, EdgeType])
    compact(edges.toList, mutable.Map.empty[Node, Node])
  }

  def this() = this(Set.empty[HyperEdge[Node, EdgeType]])

  override def clone = new CompactHyperGraph(wrapped.clone)

  def isLatest(hyperEdge: HyperEdge[Node, EdgeType]) = wrapped.isLatest(hyperEdge)
  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id]): Set[Match[Node, EdgeType, Id]] = wrapped.findSubgraphVersioned(hyperPattern)

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (contains(hyperEdge)) this
    else compact(List(hyperEdge), mutable.Map.empty[Node, Node])
  }

  override def ++=(hyperEdges: TraversableOnce[HyperEdge[Node, EdgeType]]): this.type = {
    val newEdges = hyperEdges.filter(e => !contains(e))
    compact(newEdges.toList, mutable.Map.empty[Node, Node])
  }

  override def mergeNodesInPlace(keep: Node, change: Node): CompactHyperGraph[Node, EdgeType] = {
    wrapped.mergeNodesInPlace(keep, change)
    compact((findByTarget(keep) ++ findInSources(keep)).toList, mutable.Map.empty[Node, Node])
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): CompactHyperGraph[Node, EdgeType] = {
    wrapped.mergeEdgeTypes(keep, change)
    compact(findEdges(keep).toList, mutable.Map.empty[Node, Node])
    this
  }

  def addKeepVersion(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (contains(hyperEdge)) this
    else innerCompact(List(hyperEdge), mutable.Map.empty[Node, Node])
  }

  def addAllKeepVersion(hyperEdges: TraversableOnce[HyperEdge[Node, EdgeType]]): this.type = {
    val newEdges = hyperEdges.filter(e => !contains(e))
    innerCompact(newEdges.toList, mutable.Map.empty[Node, Node])
  }

  /* --- Object Impl. --- */

  override def toString: String = f"CompactHyperGraph($edges)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], CompactHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        new CompactHyperGraph(parts.toSet)
      }
    }

  /* --- Private Methods --- */
  private def compact(hyperEdges: List[HyperEdge[Node, EdgeType]],
                      changedToKept: mutable.Map[Node, Node]): this.type = {
    wrapped.resetVersion()
    innerCompact(hyperEdges, changedToKept)
  }

  protected def innerCompact(hyperEdges: List[HyperEdge[Node, EdgeType]],
                      changedToKept: mutable.Map[Node, Node]): this.type = {
    def translateEdge(e: HyperEdge[Node, EdgeType], changedToKept: mutable.Map[Node, Node]): HyperEdge[Node, EdgeType] =
      e.copy(target = changedToKept.getOrElse(e.target, e.target), sources = e.sources.map(x => changedToKept.getOrElse(x, x)))

    for (h <- hyperEdges) {
      val hyperEdge = translateEdge(h, changedToKept)
      val foundTarget = mutable.Set.empty[Node]
      if (hyperEdge.edgeType == HyperTermIdentifier(Language.idId)) {
        foundTarget += hyperEdge.sources.head
      } else {
        val regex = HyperEdge(Hole(0), Explicit(hyperEdge.edgeType), hyperEdge.sources.map(x => Explicit(x)), EmptyMetadata)
        foundTarget ++= wrapped.findRegexHyperEdges(regex).filter(_.target != hyperEdge.target).map(_.target)
        wrapped.addKeepVersionInPlace(hyperEdge)
      }
      // Commented out as it is not always true anymore
      // Recursive will change might create a multiple target merge
//      assert(foundTarget.size <= 1)
      if (foundTarget.nonEmpty) {
        val existsTarget = foundTarget.head
        val keysToChange = changedToKept.collect({case (k, v) if v == hyperEdge.target => k})
        for (k <- keysToChange) { changedToKept(k) = existsTarget }
        val willChange = wrapped.findInSources(hyperEdge.target).map(e => translateEdge(e, changedToKept))
        wrapped.mergeNodesKeepVersionInPlace(existsTarget, hyperEdge.target)

        changedToKept(hyperEdge.target) = existsTarget
        innerCompact(willChange.toList, changedToKept)
      }
    }
    this
    // If you changed compact please run all tests with breakpoint condition on this:
    // edges.exists(e => edges.exists(e1 => e.edgeType == e1.edgeType && e.sources == e1.sources && e.target != e1.target))
  }
}

object CompactHyperGraph extends HyperGraphCompanion[CompactHyperGraph] {
  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], CompactHyperGraph[A, B]] = new mutable.ListBuffer[HyperEdge[A, B]].mapResult {
    parts => {
      new CompactHyperGraph(parts.toSet)
    }
  }

  def toJsonGraph(graph: CompactHyperGraph[HyperTermId, HyperTermIdentifier]): JsonGraph =
    JsonGraph(graph.edgesOrdered(HyperEdgeTargetOrdering).map(HyperEdge.toJsonEdge))
  def fromJsonGraph(graph: JsonGraph): CompactHyperGraph[HyperTermId, HyperTermIdentifier] =
    CompactHyperGraph(graph.edges.map(HyperEdge.toHyperEdge): _*)

  implicit val jsonGraphFormat = structures.HyperGraph.graphFormat

  def toJson(graph: CompactHyperGraph[HyperTermId, HyperTermIdentifier]): String = {
    import play.api.libs.json._
    val json = Json.toJson(toJsonGraph(graph))
    Json.prettyPrint(json)
  }

  def fromJson(text: String): CompactHyperGraph[HyperTermId, HyperTermIdentifier] = {
    import play.api.libs.json._
    fromJsonGraph(Json.fromJson[JsonGraph](Json.parse(text)).get)
  }
}
