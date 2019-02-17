package synthesis.actions.operators

import structures.immutable.HyperGraphManyWithOrderToOne
import structures.{EmptyMetadata, HyperEdge, HyperGraphManyWithOrderToOneLike, Metadata}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LocateAction.LocateMetadata
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.rewrites.{RewriteRule, RewriteSearchSpace, RewriteSearchState}
import synthesis.search.NaiveSearch
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/** Finding a hyperterm given a pattern. The given anchor will be added to the graph as a possible translation of the hyperterm.
  * @author tomer
  * @since 11/18/18
  */
class ElaborateAction(anchor: HyperTermIdentifier, goal: HyperPattern, goalRoot: TemplateTerm[HyperTermId]) extends Action {
  override def apply(state: ActionSearchState): ActionSearchState = {
    /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */

    val root = state.programs.hyperGraph.edges.find(_.edgeType == anchor).get.target
    val updatedGoal = goal.addEdge(HyperEdge(goalRoot, ExplicitTerm(anchor), List.empty, EmptyMetadata))
    def goalPredicate(state: RewriteSearchState): Boolean = state.graph.findSubgraph(updatedGoal).nonEmpty
    // Rewrite search
    val rewriteSearch = new NaiveSearch[RewriteSearchState, RewriteSearchSpace]()
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq, initialState, goalPredicate)
    val rewriteResult = rewriteSearch.search(spaceSearch)

    // Process result
    val newPrograms = Programs(rewriteResult.map(_.graph).getOrElse(state.programs.hyperGraph))
    if (rewriteResult.nonEmpty) {
      val terms = newPrograms.reconstructWithPattern(root, goal)
      if (terms.hasNext) logger.info(s"Elaborated term is ${terms.next().toString()}")
      else logger.info("Found term not constructable (probably a symbol)")
    } else logger.info("Failed to elaborate to pattern")
    ActionSearchState(newPrograms, state.rewriteRules)
  }
}

