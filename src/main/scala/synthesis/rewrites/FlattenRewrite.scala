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
    patternEdgeCreator(Hole(0), Language.applyId, Seq(Hole(1), Repetition.rep0(Int.MaxValue, Stream.from(3, 2).map(Hole.apply)).get))

  private val innerFunc: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole(1), Hole(2), Seq(Repetition.rep0(Int.MaxValue, Stream.from(4, 2).map(Hole.apply)).get))

  private val applyFuncGraph: generic.HyperGraph.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int] =
    HyperGraph(Seq(outerApply, innerFunc): _*)

  override def apply(state: RewriteSearchState, version: Long): (RewriteSearchState, Long) = {
    // Change apply to function
    // TODO: Test this
    val currentVersion = state.graph.version
    val funcResults = state.graph.findSubgraphVersioned[Int](applyFuncGraph, version)
    val newFuncEdges = for (
      (idMap, identMap) <- funcResults) yield {
      HyperEdge[HyperTermId, HyperTermIdentifier](idMap(0),
        identMap(2),
        (Stream.from(4, 2).takeWhile(s => idMap.contains(s)).map(idMap.apply) ++
          Stream.from(3, 2).takeWhile(s => idMap.contains(s)).map(idMap.apply)).toList,
        FlattenMetadata)
    }

    val newGraph = state.graph.++(newFuncEdges)
    (new RewriteSearchState(newGraph), currentVersion)
  }
}
