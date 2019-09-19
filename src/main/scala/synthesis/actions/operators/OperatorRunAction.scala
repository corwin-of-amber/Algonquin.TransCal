package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.rewrites.{RewriteSearchSpace, RewriteSearchState}
import synthesis.search.{NaiveSearch, Operator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

class OperatorRunAction(maxSearchDepth: Int, goalPredicate: Option[RewriteSearchState => Boolean] = None) extends Action {
  private val rewriteSearch = new NaiveSearch[RewriteSearchState, RewriteSearchSpace]()
  private val predicate = goalPredicate.getOrElse((_: RewriteSearchState) => false)

  private def run(searchSpace: RewriteSearchSpace) = {
    logger.debug(s"Running naive search to depth of $maxSearchDepth with predicate as $goalPredicate")
    val (success, newState) = rewriteSearch.search(searchSpace, maxSearchDepth)
    if (success) logger.debug("Found goal while running operator run")
    else logger.debug("Finished operator run to max depth")
    newState
  }

  /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
  override def apply(state: ActionSearchState): ActionSearchState = {
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq, initialState, predicate)
    val newState = run(spaceSearch)
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }

  def fromRewriteState(state: RewriteSearchState, rewriteRules: Seq[Operator[RewriteSearchState]]): RewriteSearchState = {
    val spaceSearch = new RewriteSearchSpace(rewriteRules, state, predicate)
    run(spaceSearch)
  }
}

object OperatorRunAction {
  def GenerateGoalPredicate(anchor: HyperTermIdentifier,
                            goal: HyperPattern,
                            goalRoot: TemplateTerm[HyperTermId]): RewriteSearchState => Boolean = {
    val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    state: RewriteSearchState => state.graph.findSubgraph[Int](updatedGoal).nonEmpty
  }

  def GenerateImmutableGoalPredicate(anchor: HyperTermIdentifier,
                            goal: HyperPattern,
                            goalRoot: TemplateTerm[HyperTermId]): ActionSearchState => Boolean = {
    val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    state: ActionSearchState => state.programs.hyperGraph.findSubgraph[Int](updatedGoal).nonEmpty
  }
}