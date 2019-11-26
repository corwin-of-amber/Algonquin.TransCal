package synthesis.rewrites

import structures.{EmptyMetadata, HyperEdge, Ignored}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}
import synthesis.search.{StepOperator, VersionedOperator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, TimeComplexRewriteRulesDB}
import transcallang.{Identifier, Language}

/** Creating tuple expression's time complex edge.
  *
  */
object TupleTimeComplexRewrite extends VersionedOperator[RewriteSearchState] with StepOperator[Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]], RewriteSearchState] {

  /* --- Private --- */

  private val tupleTimeComplexEdge = HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.tupleId)), Seq(RepetitionTerm.rep1(Int.MaxValue, Stream.from(1).map(ReferenceTerm(_)))), EmptyMetadata)

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

        val newMatchTimeComplexEdges = state.graph.findRegexHyperEdges(tupleTimeComplexEdge).flatMap(
          edge => {
            val tupleExpressionNodeId = edge.target
            val sourcesTimeComplexes = edge.sources.map{id =>
              val idRegex = createTimeComplexRegex(id)
              state.graph.findRegexHyperEdges(idRegex).map(_.sources(1)).iterator
            }
            Programs.combineSeq(sourcesTimeComplexes.map(_.toStream)).flatMap(timeComplexSources => {
              val (timeComplexBaseId, timeComplexEdges) = timeComplexSources.tail.foldLeft((timeComplexSources.head, Set.empty[HyperEdge[HyperTermId, HyperTermIdentifier]])){case ((baseId, edges), newId) =>
                val tupleTimeComplexId = nodeCreator.next()
                (tupleTimeComplexId, edges + HyperEdge[HyperTermId, HyperTermIdentifier](tupleTimeComplexId, HyperTermIdentifier(Identifier(TimeComplexRewriteRulesDB.ADD_TIME_COMPLEX)), Seq(baseId, newId), EmptyMetadata))
              }
              timeComplexEdges + HyperEdge(timeComplexTrueId, HyperTermIdentifier(Language.timeComplexId), Seq(tupleExpressionNodeId, timeComplexBaseId), EmptyMetadata)
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
