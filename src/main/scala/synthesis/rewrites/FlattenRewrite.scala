package synthesis.rewrites

import language.Language
import structures._
import structures.HyperGraphManyWithOrderToOneLike.{HyperEdgePattern, HyperGraphPrefix}
import structures.immutable.HyperGraphManyWithOrderToOne.HyperGraphPattern
import structures.immutable.VocabularyHyperGraph
import syntax.Identifier
import synthesis.{HyperTermId, HyperTermIdentifier, rewrites}
import synthesis.rewrites.rewrites._
import synthesis.search.Operator

object FlattenRewrite extends Operator[RewriteSearchState] {
  private val outerApply: HyperEdge[Item[HyperTermId, Int], (Item[HyperTermIdentifier, Int], Boolean)] = {
    val temp = patternEdgeCreator(Hole[HyperTermId, Int](0), Language.applyId, Seq(Hole[HyperTermId, Int](1)))
      temp.copy(edgeType = (temp.edgeType, true))
  }

  private val innerApply: HyperEdge[Item[HyperTermId, Int], (Item[HyperTermIdentifier, Int], Boolean)] = {
    val temp = patternEdgeCreator(Hole[HyperTermId, Int](1), Language.applyId, Seq(Hole[HyperTermId, Int](2)))
    temp.copy(edgeType = (temp.edgeType, true))
  }

  private val twoAppliesSubgraph: VocabularyHyperGraph[Item[HyperTermId, Int], (Item[HyperTermIdentifier, Int], Boolean)] =
    VocabularyHyperGraph[Item[HyperTermId, Int], (Item[HyperTermIdentifier, Int], Boolean)](Seq(outerApply, innerApply): _*)

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    val results = state.graph.findSubgraphPrefix[Int, HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], (Item[HyperTermIdentifier, Int], Boolean)]](twoAppliesSubgraph)
    val newEdges = for ((idMap, identMap) <- results) yield {
      val outerP: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = patternEdgeCreator(idMap(0), Language.applyId, Seq(toExplicit(idMap(1))))
      val edges = for (outer <- state.graph.findPrefix[Int](outerP.copy(edgeType = (outerP.edgeType, true)))) yield {
        val innerP: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = patternEdgeCreator(idMap(1), Language.applyId, Seq(toExplicit(idMap(2))))
        for (inner <- state.graph.findPrefix[Int](innerP.copy(edgeType = (innerP.edgeType, true)))) yield {
          HyperEdge[HyperTermId, HyperTermIdentifier](idMap(0), HyperTermIdentifier(Language.applyId), inner.sources ++ outer.sources, EmptyMetadata)
        }
      }
      edges.flatten
    }


    new RewriteSearchState(state.graph.addEdges(newEdges.flatten))
  }
}
