package synthesis.rewrites

import structures._
import structures.immutable.HyperGraph
import synthesis.rewrites.rewrites._
import synthesis.search.VersionedOperator
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Language


/** The flatten works on 2 things. If we have a leftmost apply under an apply it means the function that we will use is
  * under the lower apply. We should flatten the applies to a single one. Secondly, if we have an apply which has a
  * first parameter that isn't an apply, the function that will be used, so we should flatten it into a single function.
  */
object FlattenRewrite extends VersionedOperator[RewriteSearchState] {
  object FlattenMetadata extends Metadata {
    override protected def toStr: String = "FlattenMetadata"
  }

  private val outerApply =
    patternEdgeCreator(Hole(0), Language.applyId, Seq(Hole(1), Repetition.rep0(Int.MaxValue, Ignored()).get))

  private val innerFunc: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole(1), Hole(2), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get))

  private val applyFuncGraph: HyperGraph.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int] =
    HyperGraph(Seq(outerApply, innerFunc): _*)

  override def apply(state: RewriteSearchState, version: Long): (RewriteSearchState, Long) = {
    // TODO: may need to do this for a few times so should find an efficient way
    // TODO: Don't use filter if it is O(n)

    // Change apply to function
    val funcResults = state.graph.findSubgraphVersioned[Int](applyFuncGraph, version)
    val newFuncEdges = for (
      (idMap, identMap) <- funcResults;
      outer <- state.graph.filter(e => e.target == idMap(0) && e.sources.nonEmpty && e.sources.head == idMap(1));
      inner <- state.graph.filter(e => e.target == idMap(1) && e.edgeType == identMap(2))) yield {
      HyperEdge[HyperTermId, HyperTermIdentifier](idMap(0), identMap(2), inner.sources ++ outer.sources.drop(1), outer.metadata.merge(inner.metadata).merge(FlattenMetadata))
    }

    val newGraph = state.graph.++(newFuncEdges)
    (new RewriteSearchState(newGraph), newGraph.version)
  }
}
