package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.rewrites.{RewriteSearchSpace, RewriteSearchState}
import synthesis.search.NaiveSearch
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

  private def goalPredicate(state: RewriteSearchState): Boolean = state.graph.findSubgraph[Int](updatedGoal).nonEmpty

  override def apply(state: ActionSearchState): ActionSearchState = {
    // Rewrite search
    val newState = new OperatorRunAction(maxSearchDepth.getOrElse(Int.MaxValue), Some(goalPredicate))(state)
    val success = goalPredicate(new RewriteSearchState(newState.programs.hyperGraph))

    // Process result
    if (success) {
      val root = newState.programs.hyperGraph.findByEdgeType(anchor).head.target
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
                     predicate: RewriteSearchState => Boolean,
                     maxDepth: Double=Double.MaxValue): Option[ActionSearchState] = {
    // Rewrite search
    val rewriteSearch = new NaiveSearch[RewriteSearchState, RewriteSearchSpace]()
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq, initialState, predicate)
    val (success, newState) = rewriteSearch.search(spaceSearch, maxDepth)
    if (success) Some(ActionSearchState(Programs(newState.graph), state.rewriteRules)) else None
  }
}