package synthesis.actions.operators
import synthesis.{HyperTermId, HyperTermIdentifier}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.TemplateTerm

class ElaborateWithCaseSplit(anchor: HyperTermIdentifier,
                             goal: HyperPattern,
                             goalRoot: TemplateTerm[HyperTermId], maxSearchDepth: Option[Int] = None) extends Action {
  private val predicate = OperatorRunAction.GenerateGoalPredicate(anchor, goal, goalRoot)
  private val opRun = new OperatorRunAction(4, Some(predicate))

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newState = opRun(state)
    val rState = new RewriteSearchState(newState.programs.hyperGraph)
    if (!predicate(rState)) {
      // TODO: Add anchors for each new splitable?
      val foundToMerges = newState.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
        .map(e => {new CaseSplitAction(e).getFoundConclusions(newState)})
//      val flattened = foundToMerges.tail.foldLeft(foundToMerges.head)((s1, s2) => )
    }
    newState
  }
}
