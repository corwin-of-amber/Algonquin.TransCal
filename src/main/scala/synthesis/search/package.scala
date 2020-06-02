package synthesis

import structures.HyperEdge

package object search {
  def shiftEdges(startId: Int, edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]])
  : Set[HyperEdge[HyperTermId, HyperTermIdentifier]] =
    edges.map(e => e.copy(target = e.target.copy(e.target.id + startId),
      sources = e.sources.map(hid => hid.copy(id = hid.id + startId))))

  def edgesMax(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): Int =
    edges.flatMap(e => e.sources :+ e.target).map(_.id).max

  def mergeNodes(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]], source: HyperTermId, target: HyperTermId)
  : Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    def switch(hyperTermId: HyperTermId) = if (hyperTermId == target) source else hyperTermId

    edges.map(e =>
      if (e.sources.contains(target) || e.target == target)
        e.copy(target = switch(e.target), sources = e.sources.map(switch))
      else e
    )
  }

  import synthesis.search.rewrite.RewriteSearchState
  def disjointAppend(graphs: Seq[RewriteSearchState.HyperGraph]): RewriteSearchState.HyperGraph = {
    graphs.fold(new RewriteSearchState.HyperGraph)({
      case (g1, g2) if g1.nonEmpty => g1 ++= shiftEdges(g1.nodes.map(_.id).max, g2.edges)
      case (_, g2) => g2
    })
  }
}
