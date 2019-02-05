package synthesis

import structures.HyperEdge
import syntax.Identifier

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
