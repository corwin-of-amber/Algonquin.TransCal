package synthesis.actions.operators

import structures.{IdMetadata, Uid, UnionMetadata}
import synthesis.{HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.CaseSplitAction.SplitChooser
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator

class OperatorRunWithCaseSplit(maxSearchDepth: Int, goalPredicate: Option[RewriteSearchState => Boolean] = None,
                               splitDepth: Option[Int] = None, chooser: Option[SplitChooser] = None)
  extends SearchAction {
  private val opRun = new OperatorRunAction(4, goalPredicate)
  private val splitter = new CaseSplitAction(chooser, splitDepth, Some(maxSearchDepth))
  private val idMetadata = IdMetadata(new Uid)

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newState = fromRewriteState(new RewriteSearchState(state.programs.hyperGraph), state.rewriteRules)
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }

  override def fromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]]): RewriteSearchState = {
    var rState = state
    rState = opRun.fromRewriteState(rState, rules)
    val conclusions = splitter.getFoundConclusionsFromRewriteState(state, rules)
    rState = ObservationalEquivalence.mergeConclusions(rState, conclusions.toSeq)
    rState = opRun.fromRewriteState(rState, rules)
    rState
  }
}