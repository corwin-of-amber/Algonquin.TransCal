package synthesis.actions.operators

import structures.{IdMetadata, Uid}
import synthesis.{HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState

class OperatorRunWithCaseSplit(maxSearchDepth: Int, goalPredicate: Option[RewriteSearchState => Boolean] = None) extends Action {
  private val opRun = new OperatorRunAction(4, goalPredicate)
  private val idMetadata = IdMetadata(new Uid)

  override def apply(state: ActionSearchState): ActionSearchState = {
    val rState = new RewriteSearchState(state.programs.hyperGraph)
    var newState = opRun.fromRewriteState(rState, state.rewriteRules.toSeq)
    var caseEdges = rState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))

    while ((goalPredicate.isEmpty || !goalPredicate.get(rState)) && caseEdges.nonEmpty) {
      val e = caseEdges.head.copy(metadata = caseEdges.head.metadata.merge(idMetadata))
      newState.graph -= e
      newState.graph += e
      val toMerge = new CaseSplitAction(e).getFoundConclusionsFromRewriteState(newState, state.rewriteRules)
      newState = ObservationalEquivalence.mergeConclusions(newState, toMerge.toSeq)
      newState = opRun.fromRewriteState(newState, state.rewriteRules.toSeq)
      caseEdges = newState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
        .filterNot(_.metadata.exists(_ == idMetadata))
    }

    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }
}