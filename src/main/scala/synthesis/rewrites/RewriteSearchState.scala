package synthesis.rewrites

import structures.mutable.CompactHyperGraph
import synthesis.actions.ActionSearchState
import synthesis.search.State
import synthesis.{HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 11/18/18
  */
case class RewriteSearchState(graph: RewriteSearchState.HyperGraph) extends State[RewriteSearchState] {
  def this(graph: ActionSearchState.HyperGraph) = this(CompactHyperGraph(graph.toSeq: _*))

  override def deepCopy(): RewriteSearchState = RewriteSearchState(CompactHyperGraph(graph.toSeq: _*))
}

object RewriteSearchState {
  type HyperGraph = CompactHyperGraph[HyperTermId, HyperTermIdentifier]
}
