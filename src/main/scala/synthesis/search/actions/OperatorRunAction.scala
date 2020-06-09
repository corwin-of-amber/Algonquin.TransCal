package synthesis.search.actions

import structures.{EmptyMetadata, HyperEdge}
import synthesis.search.rewrites.IRewriteRule
import synthesis.search.rewrites.RewriteRule.HyperPattern
import synthesis.search.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.search.{ActionSearchState, NaiveSearch}
import synthesis.{HyperTermId, HyperTermIdentifier}

class OperatorRunAction(maxSearchDepth: Int, goalPredicate: Option[IRewriteRule.HyperGraph => Boolean] = None, startVersioned:Boolean = false) extends Action {
  private val predicate = goalPredicate.getOrElse((_: IRewriteRule.HyperGraph) => false)
  private val searchAction = new NaiveSearch(startVersioned = startVersioned, predicate)

  private def run(state: ActionSearchState) = {
    logger.debug(s"Running naive search to depth of $maxSearchDepth with predicate as $goalPredicate")
    val newState = searchAction(state)
    // TODO: hells no! use query graph. This is temporary, only did it because it is still an action
    var res = false
    state.updateGraph(g => res = predicate(g))
    if (res) logger.debug("Found goal while running operator run")
    else logger.debug("Finished operator run to max depth")
    newState
  }

  /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
  override def apply(state: ActionSearchState): ActionSearchState = {
    val newState = run(state)
    newState
  }
}

object OperatorRunAction {
  def GenerateGoalPredicate(anchor: HyperTermIdentifier,
                            goal: HyperPattern,
                            goalRoot: TemplateTerm[HyperTermId]): IRewriteRule.HyperGraph => Boolean = {
    val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    graph: IRewriteRule.HyperGraph => graph.findSubgraph[Int](updatedGoal).nonEmpty
  }

  def GenerateImmutableGoalPredicate(anchor: HyperTermIdentifier,
                            goal: HyperPattern,
                            goalRoot: TemplateTerm[HyperTermId]): ActionSearchState => Boolean = {
    val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    state: ActionSearchState => state.programs.queryGraph.findSubgraph[Int](updatedGoal).nonEmpty
  }
}