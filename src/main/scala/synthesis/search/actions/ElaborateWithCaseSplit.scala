package synthesis.search.actions

import structures.{IdMetadata, Uid}
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.PatternRewriteRule.HyperPattern
import synthesis.search.rewrites.Template.TemplateTerm
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

class ElaborateWithCaseSplit(anchor: HyperTermIdentifier,
                             goal: HyperPattern,
                             goalRoot: TemplateTerm[HyperTermId], maxSearchDepth: Option[Int] = None) extends Action {
  private val immutablePredicate = OperatorRunAction.GenerateImmutableGoalPredicate(anchor, goal, goalRoot)
  private val predicate = OperatorRunAction.GenerateGoalPredicate(anchor, goal, goalRoot)
  private val opRun = new OperatorRunAction(4, Some(predicate))
  private val idMetadata = IdMetadata(new Uid)

  override def apply(state: ActionSearchState): ActionSearchState = {
    var newState = opRun(state)
    var caseEdges = newState.programs.queryGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))

    while ((!immutablePredicate(newState)) && caseEdges.nonEmpty) {
      val e = caseEdges.head.copy(metadata = caseEdges.head.metadata.merge(idMetadata))
      newState.updateGraph(g => g -= e += e)
      newState = new CaseSplitAction(e, None)(newState)
      newState = opRun(newState)
      caseEdges = newState.programs.queryGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
        .filterNot(_.metadata.exists(_ == idMetadata))
    }

    newState
  }
}