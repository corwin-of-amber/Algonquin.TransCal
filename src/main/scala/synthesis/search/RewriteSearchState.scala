package synthesis.search

import structures.mutable.CompactHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 11/18/18
  */
case class RewriteSearchState(graph: RewriteSearchState.HyperGraph) extends State[RewriteSearchState] {
  def this(graph: ActionSearchState.HyperGraph) = this(CompactHyperGraph(graph.toSeq: _*))

  override def deepCopy(): RewriteSearchState = RewriteSearchState(graph.clone)
}

object RewriteSearchState {
  type HyperGraph = CompactHyperGraph[HyperTermId, HyperTermIdentifier]
}
