package synthesis.rewrites

import structures.mutable.VersionedHyperGraph
import synthesis.actions.ActionSearchState
import synthesis.search.State
import synthesis.{HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 11/18/18
  */
case class RewriteSearchState(graph: RewriteSearchState.HyperGraph) extends State {
  def this(graph: ActionSearchState.HyperGraph) = this(VersionedHyperGraph(graph.toSeq: _*))
}

object RewriteSearchState {
  type HyperGraph = VersionedHyperGraph[HyperTermId, HyperTermIdentifier]
}
