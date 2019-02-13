package synthesis.rewrites

import structures.immutable.CompactHyperGraph
import synthesis.search.State
import synthesis.{HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteSearchState(val graph: RewriteSearchState.HyperGraph) extends State

object RewriteSearchState {
  type HyperGraph = CompactHyperGraph[HyperTermId, HyperTermIdentifier]
}
