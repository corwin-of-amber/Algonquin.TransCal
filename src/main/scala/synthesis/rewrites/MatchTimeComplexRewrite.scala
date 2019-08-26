package synthesis.rewrites

import structures.{EmptyMetadata, HyperEdge, Ignored}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}
import synthesis.search.VersionedOperator
import transcallang.{Identifier, Language}

/** Creating match expression's time complex edge.
  *
  */
object MatchTimeComplexRewrite extends VersionedOperator[RewriteSearchState]{

  /* --- Private --- */

  private val matchTimeComplexEdge = HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.matchId)), Seq(RepetitionTerm.rep1(Int.MaxValue, Stream.from(1).map(ReferenceTerm(_))).get), EmptyMetadata)

  private def createTimeComplexRegex(id: HyperTermId): HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]] = {
    HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(ExplicitTerm(id), ReferenceTerm(1)), EmptyMetadata)
  }

  /* --- VersionedOperator Impl. --- */

  override def apply(state: RewriteSearchState, lastVersion: Long): (RewriteSearchState, Long) = {
    val timeComplexTrueEdge = HyperEdge(Ignored(), ExplicitTerm(HyperTermIdentifier(Language.timeComplexTrueId)), Seq.empty, EmptyMetadata)
    val timeComplexTrueIdOption = state.graph.findRegexHyperEdges(timeComplexTrueEdge).map(_.target).headOption
    timeComplexTrueIdOption match {
      case None => (state, lastVersion)
      case Some(timeComplexTrueId) =>
    val nodeCreator = Stream.from(state.graph.nodes.map(_.id).max + 1).iterator.map(HyperTermId)

    val newMatchTimeComplexEdges = state.graph.findRegexHyperEdges(matchTimeComplexEdge).flatMap(
      edge => {
        val matchExpressionNodeId = edge.target
        Programs.combineSeq(edge.sources.map(id =>
          state.graph.findRegexHyperEdges(createTimeComplexRegex(id)).map(_.sources(1)).iterator
        )).flatMap(timeComplexSources => {
          val matchTimeComplexId = nodeCreator.next()
          Set(
            HyperEdge(matchTimeComplexId, HyperTermIdentifier(Identifier("max")), timeComplexSources, EmptyMetadata),
            HyperEdge(timeComplexTrueId, HyperTermIdentifier(Language.timeComplexId), Seq(matchExpressionNodeId, matchTimeComplexId), EmptyMetadata)
          )
        })
    })
    val newGraph = state.graph ++ newMatchTimeComplexEdges
    (new RewriteSearchState(newGraph), newGraph.version)
    }
  }
}
