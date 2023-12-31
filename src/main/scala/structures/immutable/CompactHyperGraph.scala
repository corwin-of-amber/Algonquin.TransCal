package structures.immutable

import structures._
import structures.generic.HyperGraphLikeGenericCompanion
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

  lazy val version: Long = wrapped.version
  def findSubgraphVersioned[Id](hyperPattern: generic.HyperGraph.HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = wrapped.findSubgraphVersioned(hyperPattern, version)

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def +(hyperEdge: HyperEdge[Node, EdgeType]): CompactHyperGraph[Node, EdgeType] = {
    compact(List(hyperEdge))
  }

  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): CompactHyperGraph[Node, EdgeType] = {
    compact(hyperEdges.toList)
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

  @tailrec
  private def compact[Node, EdgeType](wrapped: VersionedHyperGraph[Node, EdgeType], hyperEdges: List[HyperEdge[Node, EdgeType]], changedToKept: Map[Node, Node] = Map.empty[Node, Node]): VersionedHyperGraph[Node, EdgeType] = {
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
          g = wrapped.+(hyperEdge)
          foundTarget = wrapped.findRegexHyperEdges(regex).filter(_.target != hyperEdge.target).map(_.target)
        }
        assert(foundTarget.size <= 1)
        foundTarget.headOption match {
          case Some(existsTarget) =>
            val willChange = g.findInSources[Int](hyperEdge.target).map(translateEdge)
            val merged = g.mergeNodes(existsTarget, hyperEdge.target)
            val updatedChangedToKept = changedToKept.map {
              case (change, hyperEdge.target) => (change, existsTarget)
              case t => t
            }.updated(hyperEdge.target, existsTarget)
            compact(merged, willChange.toList ++ otherHyperEdges, updatedChangedToKept)
          case _ => compact(g, otherHyperEdges, changedToKept)
        }
    }
  }
}
