package synthesis.actions.operators

import structures.{IdMetadata, Uid}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator

class ObservationalEquivalenceWithCaseSplit(maxDepth: Int = 4) extends Action {
  val idMetadata = IdMetadata(new Uid)
  val obsEquiv = new ObservationalEquivalence(maxDepth)

  def apply(state: ActionSearchState) : ActionSearchState = {
    var rState = new RewriteSearchState(state.programs.hyperGraph)
    val inital = obsEquiv.getEquivesFromRewriteState(rState, state.rewriteRules)
    rState = ObservationalEquivalence.mergeConclusions(rState, inital.toSeq)
    var caseEdges = rState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))

    while (caseEdges.nonEmpty) {
      val e = caseEdges.head.copy(metadata = caseEdges.head.metadata.merge(idMetadata))
      rState.graph -= e
      rState.graph += e
      val toMerge = new CaseSplitAction(e).getFoundConclusionsFromRewriteState(rState, state.rewriteRules)
      rState = ObservationalEquivalence.mergeConclusions(rState, toMerge.toSeq)
      caseEdges = rState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
        .filterNot(_.metadata.exists(_ == idMetadata))
    }
    ActionSearchState(Programs(rState.graph), state.rewriteRules)
  }
}