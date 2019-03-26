package structures.immutable

import structures._

import scala.annotation.tailrec
import scala.collection.mutable

/** This hyper graph keeps it self compact - EdgeType with same Nodes must go to the same target.
  * @author tomer
  * @since 11/15/18
  */
class CompactHyperGraph[Node, EdgeType] private (wrapped: HyperGraphManyWithOrderToOne[Node, EdgeType])
  extends WrapperHyperGraph[Node, EdgeType, CompactHyperGraph[Node, EdgeType]](wrapped) {

  def this(edges: Set[HyperEdge[Node, EdgeType]]) =
    this(CompactHyperGraph.compact[Node, EdgeType](VocabularyHyperGraph.empty[Node, EdgeType], edges.toList))

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): CompactHyperGraph[Node, EdgeType] = {
    compact(List(hyperEdge))
  }

  override def addEdges(hyperEdges: Set[HyperEdge[Node, EdgeType]]): CompactHyperGraph[Node, EdgeType] = {
    compact(hyperEdges.toList)
  }

  /* --- Object Impl. --- */

  override def toString: String = f"CompactHyperGraph($edges)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], CompactHyperGraph[Node, EdgeType]] =
    new mutable.LazyBuilder[HyperEdge[Node, EdgeType], CompactHyperGraph[Node, EdgeType]] {
      override def result(): CompactHyperGraph[Node, EdgeType] = {
        new CompactHyperGraph(parts.flatten.toSet)
      }
    }

  /* --- Private Methods --- */

  private def compact(hyperEdges: List[HyperEdge[Node, EdgeType]], changedToKept: Map[Node,Node]=Map.empty): CompactHyperGraph[Node, EdgeType] = {
    new CompactHyperGraph(CompactHyperGraph.compact(wrapped, hyperEdges, changedToKept))
  }
}

object CompactHyperGraph extends HyperGraphManyWithOrderToOneLikeGenericCompanion[CompactHyperGraph] {
  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], CompactHyperGraph[A, B]] = new mutable.LazyBuilder[HyperEdge[A, B], CompactHyperGraph[A, B]] {
    override def result(): CompactHyperGraph[A, B] = {
      new CompactHyperGraph(parts.flatten.toSet)
    }
  }

  /* --- Private Methods --- */

  @tailrec
  private def compact[Node, EdgeType](wrapped: HyperGraphManyWithOrderToOne[Node, EdgeType], hyperEdges: List[HyperEdge[Node, EdgeType]], changedToKept: Map[Node, Node] = Map.empty[Node, Node]): HyperGraphManyWithOrderToOne[Node, EdgeType] = {
    hyperEdges match {
      case Nil => wrapped
      case beforeChangeHyperEdge +: otherHyperEdges =>
        val hyperEdge = beforeChangeHyperEdge.copy(target = changedToKept.getOrElse(beforeChangeHyperEdge.target, beforeChangeHyperEdge.target), sources = beforeChangeHyperEdge.sources.map(x => changedToKept.getOrElse(x, x)))
        val regex = HyperEdge(Hole(0), Explicit(hyperEdge.edgeType), hyperEdge.sources.map(x => Explicit(x)), EmptyMetadata)
        val g = wrapped.addEdge(hyperEdge)
        wrapped.find(regex).headOption match {
          case Some(existsHyperEdge) if existsHyperEdge != hyperEdge =>
            val merged = g.mergeNodes(existsHyperEdge.target, hyperEdge.target)
            compact(merged, otherHyperEdges, changedToKept.updated(hyperEdge.target, existsHyperEdge.target))
          case _ => compact(g, otherHyperEdges, changedToKept)
        }
    }
  }
}
