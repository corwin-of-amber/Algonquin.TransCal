package synthesis.actions

import structures.immutable.VersionedHyperGraph
import synthesis.rewrites.RewriteSearchState
import synthesis.search.{Operator, State}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/**
  * @author tomer
  * @since 11/18/18
  */
case class ActionSearchState(programs: Programs, rewriteRules: Set[Operator[RewriteSearchState]]) extends State
object ActionSearchState {
  type HyperGraph = VersionedHyperGraph[HyperTermId, HyperTermIdentifier]

}
