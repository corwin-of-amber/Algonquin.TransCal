package synthesis.rewrites

import structures.{EmptyMetadata, HyperEdge, Ignored}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}
import synthesis.search.{StepOperator, VersionedOperator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{Identifier, Language}

/** Creating match expression's time complex edge.
  *
  */
object MatchTimeComplexRewrite extends VersionedOperator[RewriteSearchState] with StepOperator[Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]], RewriteSearchState] {

  val MAX_TIME_COMPLEX = "max"

  /* --- Private --- */

  private val matchTimeComplexEdge = HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.matchId)), Seq(RepetitionTerm.rep1(Int.MaxValue, Stream.from(1).map(ReferenceTerm(_)))), EmptyMetadata)

  private def createTimeComplexRegex(id: HyperTermId): HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]] = {
    HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(ExplicitTerm(id), ReferenceTerm(1)), EmptyMetadata)
  }

  /* --- VersionedOperator Impl. --- */


  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param state       current state from which to do the initial calculations and create an operator
    * @param lastVersion Version to use if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  override def getStep(state: RewriteSearchState, lastVersion: Long): Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]] = {
    getNewEdges(state, lastVersion).map(e=>HyperEdge(
      ExplicitTerm(e.target),
      ExplicitTerm(e.edgeType),
      e.sources.map(ExplicitTerm(_)),
      e.metadata
    ))
  }

  private def getNewEdges(state: RewriteSearchState, lastVersion: Long): Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    val timeComplexTrueEdge = HyperEdge(Ignored(), ExplicitTerm(HyperTermIdentifier(Language.timeComplexTrueId)), Seq.empty, EmptyMetadata)
    val timeComplexTrueIdOption = state.graph.findRegexHyperEdges(timeComplexTrueEdge).map(_.target).headOption
    timeComplexTrueIdOption match {
      case None => Set.empty
      case Some(timeComplexTrueId) =>
        val nodeCreator = Stream.from(state.graph.nodes.map(_.id).max + 1).iterator.map(HyperTermId)

        val newMatchTimeComplexEdges = state.graph.findRegexHyperEdges(matchTimeComplexEdge).flatMap(
          edge => {
            val matchExpressionNodeId = edge.target
            Programs.combineSeq(edge.sources.map(id =>
              state.graph.findRegexHyperEdges(createTimeComplexRegex(id)).map(_.sources(1)).toStream
            )).flatMap(timeComplexSources => {
              val matchTimeComplexId = nodeCreator.next()
              Set(
                HyperEdge(matchTimeComplexId, HyperTermIdentifier(Identifier(MAX_TIME_COMPLEX)), timeComplexSources, EmptyMetadata),
                HyperEdge(timeComplexTrueId, HyperTermIdentifier(Language.timeComplexId), Seq(matchExpressionNodeId, matchTimeComplexId), EmptyMetadata)
              )
            })
          })
        newMatchTimeComplexEdges
    }
  }

  override def apply(state: RewriteSearchState, lastVersion: Long): (RewriteSearchState, Long) = {
    state.graph ++= getNewEdges(state, lastVersion)
    (state, state.graph.version)
  }
}
