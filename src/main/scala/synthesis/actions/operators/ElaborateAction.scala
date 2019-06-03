package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.rewrites.{RewriteSearchSpace, RewriteSearchState}
import synthesis.search.NaiveSearch
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/** Finding a hyperterm given a pattern. The given anchor will be added to the graph as a possible translation of the hyperterm.
  *
  * @author tomer
  * @since 11/18/18
  */
class ElaborateAction(anchor: HyperTermIdentifier, goal: HyperPattern, goalRoot: TemplateTerm[HyperTermId], maxSearchDepth: Option[Int] = None) extends Action {
  override def apply(state: ActionSearchState): ActionSearchState = {
    /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */

    val updatedGoal = goal.addEdge(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))

    def goalPredicate(state: RewriteSearchState): Boolean = state.graph.findSubgraph[Int](updatedGoal).nonEmpty

    // Rewrite search
    val newState = maxSearchDepth.map(d => ElaborateAction.RunNaiveSearch(state, goalPredicate, d)).getOrElse(ElaborateAction.RunNaiveSearch(state, goalPredicate))

    // Process result
    val newPrograms = newState.map(_.programs).getOrElse(state.programs)
    if (newState.nonEmpty) {
      val root = newPrograms.hyperGraph.edges.find(_.edgeType == anchor).get.target
      val terms = newPrograms.reconstructWithPattern(root, goal, Some(goalRoot))
      if (terms.hasNext) logger.info(s"Elaborated term is '${Programs.termToString(terms.next())}'")
      else logger.info("Found term not constructable (probably a symbol)")
    } else logger.info("Failed to elaborate to pattern")
    newState.getOrElse(state)
  }
}

object ElaborateAction {
  def RunNaiveSearch(state: ActionSearchState, predicate: RewriteSearchState => Boolean, maxDepth: Double=Double.MaxValue): Option[ActionSearchState] = {
    // Rewrite search
    val rewriteSearch = new NaiveSearch[RewriteSearchState, RewriteSearchSpace]()
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq, initialState, predicate)
    rewriteSearch.search(spaceSearch, maxDepth).map(s => ActionSearchState(Programs(s.graph), state.rewriteRules))
  }
}