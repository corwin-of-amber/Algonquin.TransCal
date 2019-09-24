package synthesis.actions.operators

import structures.{IdMetadata, Uid}
import synthesis.{HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator

class OperatorRunWithCaseSplit(maxSearchDepth: Int, goalPredicate: Option[RewriteSearchState => Boolean] = None) extends SearchAction {
  private val opRun = new OperatorRunAction(4, goalPredicate)
  private val idMetadata = IdMetadata(new Uid)

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newState = fromRewriteState(new RewriteSearchState(state.programs.hyperGraph), state.rewriteRules)
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }

  override def fromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]]): RewriteSearchState = {
    var rState = state
    var newState = opRun.fromRewriteState(rState, rules)
    var caseEdges = newState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))

    while ((goalPredicate.isEmpty || !goalPredicate.get(rState)) && caseEdges.nonEmpty) {
      val e = caseEdges.head.copy(metadata = caseEdges.head.metadata.merge(idMetadata))
      newState.graph -= e
      newState.graph += e
      val toMerge = new CaseSplitAction(e).getFoundConclusionsFromRewriteState(newState, rules)
      newState = ObservationalEquivalence.mergeConclusions(newState, toMerge.toSeq)
      newState = opRun.fromRewriteState(newState, rules)
      caseEdges = newState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
        .filterNot(_.metadata.exists(_ == idMetadata))
    }

    newState
  }
}