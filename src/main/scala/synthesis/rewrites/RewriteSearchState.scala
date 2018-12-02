package synthesis.rewrites

import structures.HyperGraphManyWithOrderToOne
import synthesis.HyperTerm
import synthesis.search.State

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteSearchState(val graph: RewriteSearchState.HyperGraph) extends State

object RewriteSearchState {
  type HyperGraph = HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm]
}
