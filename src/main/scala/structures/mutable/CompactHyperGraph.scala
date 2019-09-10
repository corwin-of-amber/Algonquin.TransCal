package structures.mutable

import structures._
import structures.generic.HyperGraph.HyperGraphPattern
import synthesis.HyperTermIdentifier
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

  def version: Long = wrapped.version
  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = wrapped.findSubgraphVersioned(hyperPattern, version)

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    compact(List(hyperEdge), mutable.Map.empty[Node, Node])
  }

  override def ++=(hyperEdges: TraversableOnce[HyperEdge[Node, EdgeType]]): this.type = {
    compact(hyperEdges.toList, mutable.Map.empty[Node, Node])
  }

  override def mergeNodes(keep: Node, change: Node): CompactHyperGraph[Node, EdgeType] = {
    wrapped.mergeNodes(keep, change)
    compact((findByTarget(keep) ++ findInSources(keep)).toList, mutable.Map.empty[Node, Node])
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): CompactHyperGraph[Node, EdgeType] = {
    wrapped.mergeEdgeTypes(keep, change)
    compact(findEdges(keep).toList, mutable.Map.empty[Node, Node])
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
        wrapped.+=(hyperEdge)
      }
      // Commented out as it is not always true anymore
//      assert(foundTarget.size <= 1)
      if (foundTarget.nonEmpty) {
        val existsTarget = foundTarget.head
        val keysToChange = changedToKept.collect({case (k, v) if v == hyperEdge.target => k})
        for (k <- keysToChange) { changedToKept(k) = existsTarget }
        val willChange = wrapped.findInSources[Int](hyperEdge.target).map(e => translateEdge(e, changedToKept))
        wrapped.mergeNodes(existsTarget, hyperEdge.target)

        changedToKept(hyperEdge.target) = existsTarget
        compact(willChange.toList, changedToKept)
      }
    }
    this
    // If you changed compact please runn all tests with breakpoint condition on this:
    // edges.exists(e => edges.exists(e1 => e.edgeType == e1.edgeType && e.sources == e1.sources && e.target != e1.target))
  }
}

object CompactHyperGraph extends HyperGraphLikeGenericCompanion[CompactHyperGraph] {
  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], CompactHyperGraph[A, B]] = new mutable.ListBuffer[HyperEdge[A, B]].mapResult {
    parts => {
      new CompactHyperGraph(parts.toSet)
    }
  }
}
