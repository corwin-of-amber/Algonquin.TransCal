package synthesis.search.action.operators

import structures.{IdMetadata, Uid}
import synthesis.Programs
import synthesis.search.action.operators.CaseSplitAction.SplitChooser
import synthesis.search.Operator
import synthesis.search.action.ActionSearchState
import synthesis.search.rewrite.RewriteSearchState

class OperatorRunWithCaseSplit(maxSearchDepth: Int, goalPredicate: Option[RewriteSearchState => Boolean] = None,
                               preRunDepth: Option[Int] = None, splitDepth: Option[Int] = None, chooser: Option[SplitChooser] = None, startVersioned: Boolean=false)
  extends SearchAction {
  private val opRun = new OperatorRunAction(preRunDepth.getOrElse(2), goalPredicate, startVersioned=startVersioned)
  private val splitter = new CaseSplitAction(chooser, splitDepth, Some(maxSearchDepth), preRunDepth, startVersioned=startVersioned)
  private val idMetadata = IdMetadata(new Uid)

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newState = fromRewriteState(new RewriteSearchState(state.programs.hyperGraph), state.rewriteRules)
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }

  override def fromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]]): RewriteSearchState = {
    var rState = state
    val conclusions = splitter.getFoundConclusionsFromRewriteState(rState, rules)
    rState = ObservationalEquivalence.mergeConclusions(rState, conclusions.toSeq)
    if (conclusions.exists(_.size > 2))
      rState = opRun.fromRewriteState(rState, rules)
    rState
  }
}