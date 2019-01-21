package synthesis.rewrites

import language.Language
import structures._
import structures.immutable.VocabularyHyperGraph
import synthesis.rewrites.rewrites._
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier}

object FlattenRewrite extends Operator[RewriteSearchState] {
  private val outerApply: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole[HyperTermId, Int](0), Language.applyId, Seq(Hole[HyperTermId, Int](1), Repetition.rep0[HyperTermId, Int](Int.MaxValue, Ignored[HyperTermId, Int]()).get))

  private val innerApply: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole[HyperTermId, Int](1), Language.applyId, Seq(Hole[HyperTermId, Int](2), Repetition.rep0[HyperTermId, Int](Int.MaxValue, Ignored[HyperTermId, Int]()).get))

  private val twoAppliesSubgraph: VocabularyHyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    VocabularyHyperGraph(Seq(outerApply, innerApply): _*)

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    val results = state.graph.findSubgraph(twoAppliesSubgraph)
    val newEdges = for (
      (idMap, _) <- results;
      outer <-state.graph.filter(e => e.target == idMap(0) && e.sources.head == idMap(1));
      inner <- state.graph.filter(e => e.target == idMap(1) && e.sources.head == idMap(2))) yield {
      HyperEdge[HyperTermId, HyperTermIdentifier](idMap(0), HyperTermIdentifier(Language.applyId), inner.sources ++ outer.sources.drop(1), EmptyMetadata)
    }


    new RewriteSearchState(state.graph.addEdges(newEdges))
  }
}
