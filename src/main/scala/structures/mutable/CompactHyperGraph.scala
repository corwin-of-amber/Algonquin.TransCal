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
    compact(edges.toList, mutable.Map.empty[Node, Node])
  }

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def +(hyperEdge: HyperEdge[Node, EdgeType]): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(wrapped).compact(List(hyperEdge), mutable.Map.empty[Node, Node])
  }

  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(wrapped).compact(hyperEdges.toList, mutable.Map.empty[Node, Node])
  }

  override def +=(hyperEdge: HyperEdge[Node, EdgeType]): Unit = {
    compact(List(hyperEdge), mutable.Map.empty[Node, Node])
  }

  override def ++=(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): Unit = {
    compact(hyperEdges.toList, mutable.Map.empty[Node, Node])
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
                      changedToKept: mutable.Map[Node, Node]): CompactHyperGraph[Node, EdgeType] = {
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
      assert(foundTarget.size <= 1)
      if (foundTarget.nonEmpty) {
        val existsTarget = foundTarget.head
        // TODO: use find regex
        for ((k, v) <- changedToKept if v == hyperEdge.target) {
          changedToKept(k) = existsTarget
        }
        // TODO: Use rep0 to create a fast pattern for this
        val willChange = wrapped.edges.filter(_.sources.contains(hyperEdge.target)).map(e => translateEdge(e, changedToKept))
        wrapped.mergeNodes(existsTarget, hyperEdge.target)

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
