package synthesis.actions.operators

import structures.{IdMetadata, Uid, UnionMetadata}
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
    val caseEdges = newState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
    // We want to prevent too many split steps so we will freeze case edges by using metadata
    var markedEdges = caseEdges.map(_.copy(metadata = caseEdges.head.metadata.merge(idMetadata)))
    newState.graph --= markedEdges
    newState.graph ++= markedEdges

    while ((goalPredicate.isEmpty || !goalPredicate.get(rState)) && markedEdges.nonEmpty) {
      val e = markedEdges.head.copy(metadata = UnionMetadata(markedEdges.head.metadata.filterNot(_ == idMetadata).toSet))
      newState.graph -= e
      newState.graph += e
      val toMerge = new CaseSplitAction(e).getFoundConclusionsFromRewriteState(newState, rules)
      newState = ObservationalEquivalence.mergeConclusions(newState, toMerge.toSeq)
      newState = opRun.fromRewriteState(newState, rules)
      markedEdges = newState.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
        .filter(_.metadata.exists(_ == idMetadata))
    }

    newState
  }
}