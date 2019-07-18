package structures.mutable

import structures._
import synthesis.HyperTermIdentifier
import transcallang.Language

import scala.collection.{GenTraversableOnce, mutable}

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  *
  * @author tomer
  * @since 11/15/18
  */
class CompactHyperGraph[Node, EdgeType] private(wrapped: HyperGraph[Node, EdgeType])
  extends WrapperHyperGraph[Node, EdgeType, CompactHyperGraph[Node, EdgeType]](wrapped) {

  def this(edges: Set[HyperEdge[Node, EdgeType]]) = {
    this(VocabularyHyperGraph.empty[Node, EdgeType])
    compact(edges.toList)
  }

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def +(hyperEdge: HyperEdge[Node, EdgeType]): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(wrapped).compact(List(hyperEdge))
  }

  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(wrapped).compact(hyperEdges.toList)
  }

  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): Unit = {
    compact(List(hyperEdge))
  }

  override def ++=(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): Unit = {
    compact(hyperEdges.toList)
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
                      changedToKept: mutable.Map[Node, Node] = mutable.Map.empty[Node, Node]): CompactHyperGraph[Node, EdgeType] = {
    def translateEdge(e: HyperEdge[Node, EdgeType]): HyperEdge[Node, EdgeType] =
      e.copy(target = changedToKept.getOrElse(e.target, e.target), sources = e.sources.map(x => changedToKept.getOrElse(x, x)))

    for (h <- hyperEdges; hyperEdge = translateEdge(h)) {
      val foundTarget = mutable.Set.empty[Node]
      if (hyperEdge.edgeType == HyperTermIdentifier(Language.idId)) {
        foundTarget += hyperEdge.sources.head
      } else {
        val regex = HyperEdge(Hole(0), Explicit(hyperEdge.edgeType), hyperEdge.sources.map(x => Explicit(x)), EmptyMetadata)
        foundTarget ++= wrapped.findRegexHyperEdges(regex).filter(_.target != hyperEdge.target).map(_.target)
        wrapped.+=(hyperEdge)
      }
      assert(foundTarget.size <= 1)
      if (foundTarget.nonEmpty) {
        val existsTarget = foundTarget.head
        // TODO: use find regex
        val willChange = wrapped.edges.filter(_.sources.contains(hyperEdge.target)).map(translateEdge)
        wrapped.mergeNodes(existsTarget, hyperEdge.target)
        for ((k, v) <- changedToKept if v == hyperEdge.target) {
          changedToKept(k) = existsTarget
        }
        changedToKept(hyperEdge.target) = existsTarget
        compact(willChange.toList, changedToKept)
      }
    }
    this
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
