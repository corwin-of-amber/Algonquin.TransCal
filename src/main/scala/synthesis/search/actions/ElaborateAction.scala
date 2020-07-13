package synthesis.search.actions

import structures.{EmptyMetadata, HyperEdge}
import synthesis.search.{ActionSearchState, NaiveSearch}
import synthesis.search.rewrites.PatternRewriteRule.HyperPattern
import synthesis.search.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/** Finding a hyperterm given a pattern.
  * The given anchor will be added to the graph as a possible translation of the hyperterm.
  *
  * @author tomer
  * @since 11/18/18
  */
class ElaborateAction(anchor: HyperTermIdentifier,
                      goal: HyperPattern,
                      goalRoot: TemplateTerm[HyperTermId], maxSearchDepth: Option[Int] = None) extends Action {
  /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
  private val updatedGoal = goal.+(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))

  private def goalPredicate(state: structures.generic.HyperGraph[HyperTermId, HyperTermIdentifier]): Boolean = state.findSubgraph[Int](updatedGoal).nonEmpty

  override def apply(state: ActionSearchState): ActionSearchState = {
    // Rewrite search
    val newState = new OperatorRunAction(Some(goalPredicate))(state, maxSearchDepth.map(_.toDouble))
    val success = goalPredicate(newState.programs.queryGraph)

    // Process result
    if (success) {
      val root = newState.programs.queryGraph.findByEdgeType(anchor).head.target
      val terms = newState.programs.reconstructWithPattern(root, goal, Some(goalRoot))
      if (terms.nonEmpty) logger.info(s"Elaborated term is '${Programs.termToString(terms.head)}'")
      else logger.info("Found term not constructable (probably a symbol)")
      newState
    } else {
      logger.info("Failed to elaborate to pattern")
      state
    }
  }
}

object ElaborateAction {
  def RunNaiveSearch(state: ActionSearchState,
                     predicate: structures.generic.HyperGraph[HyperTermId, HyperTermIdentifier] => Boolean,
                     maxDepth: Double): Option[ActionSearchState] = {
    // Rewrite search
    val searchAction = new NaiveSearch(isGoal = g => predicate(g))
    val newState = searchAction(state, Some(maxDepth))
    if (predicate(newState.programs.queryGraph)) Some(newState) else None
  }
}