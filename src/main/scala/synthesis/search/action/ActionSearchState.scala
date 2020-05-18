package synthesis.search.action

import structures.immutable.CompactHyperGraph
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.{Operator, State}
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
