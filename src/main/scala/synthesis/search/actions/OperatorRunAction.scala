package synthesis.search.actions

import structures.{EmptyMetadata, HyperEdge}
import synthesis.search.rewrites.RewriteRule
import synthesis.search.rewrites.PatternRewriteRule.HyperPattern
import synthesis.search.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.search.{ActionSearchState, DepthAwareSearch, NaiveSearch}
import synthesis.{HyperTermId, HyperTermIdentifier}

class OperatorRunAction(maxSearchDepth: Int, goalPredicate: Option[RewriteRule.HyperGraph => Boolean] = None, startVersioned:Boolean = false, edgeDepth: Option[Int] = None) extends SearchAction {
  private val predicate = goalPredicate.getOrElse((_: RewriteRule.HyperGraph) => false)
  private val searchAction = new DepthAwareSearch(maxSearchDepth, edgeDepth, startVersioned, predicate)

  /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
  override def apply(state: ActionSearchState, depth: Double): ActionSearchState = {
    logger.debug(s"Running naive search to depth of $maxSearchDepth with predicate as $goalPredicate")
    val newState = searchAction(state, depth)
    // TODO: hells no! use query graph. This is temporary, only did it because it is still an action
    var res = false
    state.updateGraph(g => res = predicate(g))
    if (res) logger.debug("Found goal while running operator run")
    else logger.debug("Finished operator run to max depth")
    newState
  }
}

object OperatorRunAction {
  def GenerateGoalPredicate(anchor: HyperTermIdentifier,
                            goal: HyperPattern,
                            goalRoot: TemplateTerm[HyperTermId]): RewriteRule.HyperGraph => Boolean = {
    val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    graph: RewriteRule.HyperGraph => graph.findSubgraph[Int](updatedGoal).nonEmpty
  }

  def GenerateImmutableGoalPredicate(anchor: HyperTermIdentifier,
                            goal: HyperPattern,
                            goalRoot: TemplateTerm[HyperTermId]): ActionSearchState => Boolean = {
    val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    state: ActionSearchState => state.programs.queryGraph.findSubgraph[Int](updatedGoal).nonEmpty
  }
}