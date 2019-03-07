package synthesis.rewrites

import transcallang.Language
import structures._
import structures.immutable.{HyperGraphManyWithOrderToOne, VersionedHyperGraph, VocabularyHyperGraph}
import synthesis.rewrites.rewrites._
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier}


/** The flatten works on 2 things. If we have a leftmost apply under an apply it means the function that we will use is
  * under the lower apply. We should flatten the applies to a single one. Secondly, if we have an apply which has a
  * first parameter that isn't an apply, the function that will be used, so we should flatten it into a single function.
  */
object FlattenRewrite extends Operator[RewriteSearchState] {
  object FlattenMetadata extends Metadata {
    override protected def toStr: String = "FlattenMetadata"
  }

  private val outerApply = patternEdgeCreator(Hole(0), Language.applyId, Seq(Hole(1), Repetition.rep0(Int.MaxValue, Ignored()).get))

  private val innerFunc: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole(1), Hole(2), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get))

  private val applyFuncGraph: HyperGraphManyWithOrderToOne.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int] =
    VocabularyHyperGraph(Seq(outerApply, innerFunc): _*)

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    // TODO: may need to do this for a few times so should find an efficient way
    // TODO: Don't use filter if it is O(n)

    // Change apply to function
    val funcResults = state.graph.findSubgraphVersioned[Int](applyFuncGraph, VersionedHyperGraph.STATIC_VERSION)
    val newFuncEdges = for (
      (idMap, identMap) <- funcResults;
      outer <- state.graph.filter(e => e.target == idMap(0) && e.sources.head == idMap(1));
      inner <- state.graph.filter(e => e.target == idMap(1) && e.edgeType == identMap(2))) yield {
      HyperEdge[HyperTermId, HyperTermIdentifier](idMap(0), identMap(2), inner.sources ++ outer.sources.drop(1), outer.metadata.merge(inner.metadata).merge(FlattenMetadata))
    }

    new RewriteSearchState(state.graph.addEdges(newFuncEdges))
  }
}
