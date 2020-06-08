package synthesis.search

import structures.immutable.CompactHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/**
  * @author tomer
  * @since 11/18/18
  */
case class ActionSearchState(programs: Programs, rewriteRules: Set[Operator[RewriteSearchState]]) extends State[ActionSearchState] {
  override def deepCopy(): ActionSearchState = this.copy()
}

object ActionSearchState {
  type HyperGraph = CompactHyperGraph[HyperTermId, HyperTermIdentifier]
}
