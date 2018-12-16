package synthesis.actions.operators

import relentless.rewriting.RuleBasedTactic.{Markers, ⇢}
import relentless.rewriting.{RevisionDiff, Rules}
import syntax.{Identifier, Scheme, Tree}
import synthesis.HyperTermIdentifier
import synthesis.actions.ActionSearchState

/** IS THIS REALLY NEEDED IN CASE WE KEEP PROGRAMS?
  * @author tomer
  * @since 11/18/18
  */
class ElaborateAction(goalScheme: Scheme) extends Action {
  override def apply(state: ActionSearchState): ActionSearchState = {

    val work = state.programs

    state.programs.hyperGraph.findEdges(HyperTermIdentifier(Markers.goal.leaf)).headOption match {
      case Some(hyperEdge) =>
        val elaborated = state.programs.reconstruct(hyperEdge.edgeType).map(goalScheme(_)).next()
        val original = state.programs.reconstruct(hyperEdge.target).toStream.headOption getOrElse new Tree[Identifier](new Identifier("?"))
        val t = (RevisionDiff(List(), List(), List(original ⇢ elaborated)), Rules.empty)  // Equal for adding two terms to the same hyper term
        new ActionSearchState(null, state.rewriteRules)
      case None => state
    }
  }
}

