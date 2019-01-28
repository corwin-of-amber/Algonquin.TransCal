package synthesis.rewrites

import language.Language
import structures._
import structures.immutable.VocabularyHyperGraph
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

  private val outerApply: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole[HyperTermId, Int](0), Language.applyId, Seq(Hole[HyperTermId, Int](1), Repetition.rep0[HyperTermId, Int](Int.MaxValue, Ignored[HyperTermId, Int]()).get))

  private val innerFunc: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole[HyperTermId, Int](1), Hole[HyperTermIdentifier, Int](2), Seq(Repetition.rep0[HyperTermId, Int](Int.MaxValue, Ignored[HyperTermId, Int]()).get))

  private val applyFuncGraph: HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    VocabularyHyperGraph(Seq(outerApply, innerFunc): _*)

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    // TODO: may need to do this for a few times so should find an efficient way
    // TODO: Don't use filter if it is O(n)

    // Change apply to function
    val funcResults = state.graph.findSubgraph(applyFuncGraph)
    val newFuncEdges = for (
      (idMap, identMap) <- funcResults;
      outer <- state.graph.filter(e => e.target == idMap(0) && e.sources.head == idMap(1));
      inner <- state.graph.filter(e => e.target == idMap(1) && e.edgeType == identMap(2))) yield {
      HyperEdge[HyperTermId, HyperTermIdentifier](idMap(0), identMap(2), inner.sources ++ outer.sources.drop(1), outer.metadata.merge(inner.metadata).merge(FlattenMetadata))
    }

    new RewriteSearchState(state.graph.addEdges(newFuncEdges))
  }
}
