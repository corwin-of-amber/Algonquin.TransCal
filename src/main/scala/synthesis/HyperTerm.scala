package synthesis

import structures.{Explicit, Hole, HyperEdge, Item}
import transcallang.Identifier

/**
  * @author tomer
  * @since 11/16/18
  */
sealed trait HyperTerm
final case class HyperTermId(id: Int) extends HyperTerm
final case class HyperTermIdentifier(identifier: Identifier) extends HyperTerm

object HyperEdgeTargetOrdering extends Ordering[HyperEdge[HyperTermId, HyperTermIdentifier]] {
  override def compare(x: HyperEdge[HyperTermId, HyperTermIdentifier], y: HyperEdge[HyperTermId, HyperTermIdentifier]): Int = x.target.id compare y.target.id
}

object PatternEdgeTargetOrdering extends Ordering[HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]] {
  override def compare(x: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]], y: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]): Int =
    (x.target, y.target) match {
      case (_: Hole[HyperTermId, Int], _: Explicit[HyperTermId, Int]) => -1
      case (_: Explicit[HyperTermId, Int], _: Hole[HyperTermId, Int]) => 1
      case (a: Hole[HyperTermId, Int], b: Hole[HyperTermId, Int]) => a.id.compare(b.id)
      case (a: Explicit[HyperTermId, Int], b: Explicit[HyperTermId, Int]) => a.value.id.compare(b.value.id)
    }
}