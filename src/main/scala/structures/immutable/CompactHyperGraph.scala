package structures.immutable

import structures._
import structures.generic.HyperGraphLikeGenericCompanion
import structures.immutable.CompactHyperGraph.innerCompact
import synthesis.HyperTermIdentifier
import transcallang.Language

import scala.annotation.tailrec
import scala.collection.{GenTraversableOnce, mutable}

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  * @author tomer
  * @since 11/15/18
  */
class CompactHyperGraph[Node, EdgeType] private (wrapped: VersionedHyperGraph[Node, EdgeType])
  extends WrapperHyperGraph[Node, EdgeType, CompactHyperGraph[Node, EdgeType]](wrapped) {

  def this(edges: Set[HyperEdge[Node, EdgeType]]) =
    this(CompactHyperGraph.compact[Node, EdgeType](VersionedHyperGraph.empty[Node, EdgeType], edges.toList))

  def this() = this(Set.empty[HyperEdge[Node, EdgeType]])

  // TODO: merge versioned and compact or haave an inheritence
  def isLatest(hyperEdge: HyperEdge[Node, EdgeType]) = wrapped.isLatest(hyperEdge)
  def findSubgraphVersioned[Id](hyperPattern: generic.HyperGraph.HyperGraphPattern[Node, EdgeType, Id]): Set[(Map[Id, Node], Map[Id, EdgeType])] = wrapped.findSubgraphVersioned(hyperPattern)

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def +(hyperEdge: HyperEdge[Node, EdgeType]): CompactHyperGraph[Node, EdgeType] = {
    compact(List(hyperEdge))
  }

  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): CompactHyperGraph[Node, EdgeType] = {
    compact(hyperEdges.toList)
  }

  def addKeepVersion(hyperEdge: HyperEdge[Node, EdgeType]): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(innerCompact(wrapped, List(hyperEdge), Map.empty))
  }

  def addAllKeepVersion(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(innerCompact(wrapped, hyperEdges.toList, Map.empty))
  }

  /* --- Object Impl. --- */

  override lazy val toString: String = f"CompactHyperGraph($edges)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], CompactHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        new CompactHyperGraph(parts.toSet)
      }
    }

  /* --- Private Methods --- */

  private def compact(hyperEdges: List[HyperEdge[Node, EdgeType]], changedToKept: Map[Node,Node]=Map.empty): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(CompactHyperGraph.compact(wrapped, hyperEdges, changedToKept))
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

  /* --- Private Methods --- */
  private def compact[Node, EdgeType](wrapped: VersionedHyperGraph[Node, EdgeType],
                                      hyperEdges: List[HyperEdge[Node, EdgeType]],
                                      changedToKept: Map[Node, Node] = Map.empty[Node, Node])
  : VersionedHyperGraph[Node, EdgeType] = {
    innerCompact(wrapped.resetVersion(), hyperEdges, changedToKept)
  }

  @tailrec
  private def innerCompact[Node, EdgeType](wrapped: VersionedHyperGraph[Node, EdgeType], hyperEdges: List[HyperEdge[Node, EdgeType]], changedToKept: Map[Node, Node] = Map.empty[Node, Node]): VersionedHyperGraph[Node, EdgeType] = {
    hyperEdges match {
      case Nil => wrapped
      case beforeChangeHyperEdge +: otherHyperEdges =>
        def translateEdge(e: HyperEdge[Node, EdgeType]): HyperEdge[Node, EdgeType] =
          e.copy(target = changedToKept.getOrElse(e.target, e.target), sources = e.sources.map(x => changedToKept.getOrElse(x, x)))
        val hyperEdge = translateEdge(beforeChangeHyperEdge)
        var foundTarget = Set[Node]()
        var g = wrapped
        if (hyperEdge.edgeType == HyperTermIdentifier(Language.idId)) {
          // "id" Is a special function that says target == source and has a single source therefor the merge
          foundTarget = Set(hyperEdge.sources.head)
        } else {
          val regex = HyperEdge(Hole(0), Explicit(hyperEdge.edgeType), hyperEdge.sources.map(x => Explicit(x)), EmptyMetadata)
          g = wrapped.addKeepVersion(hyperEdge)
          foundTarget = wrapped.findRegexHyperEdges(regex).filter(_.target != hyperEdge.target).map(_.target)
        }
        // Recursive will change might create a multiple target merge
//        assert(foundTarget.size <= 1)
        foundTarget.headOption match {
          case Some(existsTarget) =>
            val willChange = g.findInSources(hyperEdge.target).map(translateEdge)
            val merged = g.mergeNodesKeepVersion(existsTarget, hyperEdge.target)
            val updatedChangedToKept = changedToKept.map {
              case (change, hyperEdge.target) => (change, existsTarget)
              case t => t
            }.updated(hyperEdge.target, existsTarget)
            innerCompact(merged, willChange.toList ++ otherHyperEdges, updatedChangedToKept)
          case _ => innerCompact(g, otherHyperEdges, changedToKept)
        }
    }
  }
}
