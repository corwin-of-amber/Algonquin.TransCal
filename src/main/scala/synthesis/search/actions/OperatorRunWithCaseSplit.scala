package synthesis.search.actions

import structures.{IdMetadata, Uid}
import synthesis.Programs
import CaseSplitAction.SplitChooser
import synthesis.search.rewrites.RewriteRule
import synthesis.search.{ActionSearchState, Operator}

class OperatorRunWithCaseSplit(maxSearchDepth: Int, goalPredicate: Option[RewriteRule.HyperGraph => Boolean] = None,
                               preRunDepth: Option[Int] = None, splitDepth: Option[Int] = None, chooser: Option[SplitChooser] = None, startVersioned: Boolean=false)
  extends Action {
  private val opRun = new OperatorRunAction(preRunDepth.getOrElse(2), goalPredicate, startVersioned=startVersioned)
  private val splitter = new CaseSplitAction(chooser, splitDepth, Some(maxSearchDepth), preRunDepth, startVersioned=startVersioned)
  private val idMetadata = IdMetadata(new Uid)

  override def apply(state: ActionSearchState): ActionSearchState = {
    var rState = state
    val conclusions = splitter.getFoundConclusions(rState)
    rState = ObservationalEquivalence.mergeConclusions(rState, conclusions.toSeq)
    if (conclusions.exists(_.size > 2))
      rState = opRun(rState)
    rState
  }
}