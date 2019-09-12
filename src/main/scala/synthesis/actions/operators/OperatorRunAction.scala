package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.{RewriteSearchSpace, RewriteSearchState}
import synthesis.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.search.NaiveSearch

class OperatorRunAction(maxSearchDepth: Int, goalPredicate: Option[RewriteSearchState => Boolean] = None) extends Action {
  /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
  override def apply(state: ActionSearchState): ActionSearchState = {
    val predicate = goalPredicate.getOrElse((_: RewriteSearchState) => false)

    // Rewrite search
    val rewriteSearch = new NaiveSearch[RewriteSearchState, RewriteSearchSpace]()
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq, initialState, predicate)
    logger.debug(s"Running naive search to depth of $maxSearchDepth with predicate as $goalPredicate")
    val (success, newState) = rewriteSearch.search(spaceSearch, maxSearchDepth)
    if (success) logger.debug("Found goal while running operator run")
    else logger.debug("Finished operator run to max depth")
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }
}

object OperatorRunAction {
  def GenerateGoalPredicate(anchor: HyperTermIdentifier,
                            goal: HyperPattern,
                            goalRoot: TemplateTerm[HyperTermId]): RewriteSearchState => Boolean = {
    val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    state: RewriteSearchState => state.graph.findSubgraph[Int](updatedGoal).nonEmpty
  }
}