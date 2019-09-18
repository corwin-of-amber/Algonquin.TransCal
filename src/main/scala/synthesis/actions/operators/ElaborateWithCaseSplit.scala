package synthesis.actions.operators
import structures.Metadata
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.ElaborateWithCaseSplit.{IdMetadata, Uid}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.TemplateTerm
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
    var caseEdges = newState.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))

    while ((!immutablePredicate(newState)) && caseEdges.nonEmpty) {
      val e = caseEdges.head.copy(metadata = caseEdges.head.metadata.merge(idMetadata))
      newState = newState.copy(programs = Programs(newState.programs.hyperGraph - e + e))
      newState = new CaseSplitAction(e)(newState)
      newState = opRun(newState)
      caseEdges = newState.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
        .filterNot(_.metadata.exists(_ == idMetadata))
    }

    newState
  }
}

object ElaborateWithCaseSplit {
  class Uid {}
  case class IdMetadata(uid: Uid) extends Metadata {
    override protected def toStr: String = s"IdMetadata($uid)"
  }
}