package synthesis.rewrites

import structures.{EmptyMetadata, HyperEdge, Ignored}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}
import synthesis.search.VersionedOperator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, TimeComplexRewriteRulesDB}
import transcallang.{Identifier, Language}

/** Creating tuple expression's time complex edge.
  *
  */
object TupleTimeComplexRewrite extends VersionedOperator[RewriteSearchState]{

  /* --- Private --- */

  private val tupleTimeComplexEdge = HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.tupleId)), Seq(RepetitionTerm.rep1(Int.MaxValue, Stream.from(1).map(ReferenceTerm(_)))), EmptyMetadata)

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
    val newGraph = state.graph ++ newMatchTimeComplexEdges
    (new RewriteSearchState(newGraph), newGraph.version)
    }
  }
}
