package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge, Metadata}
import structures.immutable.HyperGraphManyWithOrderToOne
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.{Category, HyperPattern}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.rewrites.{RewriteRule, RewriteSearchSpace, RewriteSearchState}
import synthesis.search.BreadthFirstSearch

/** Finding a hyperterm given a pattern. The given anchor will be added to the graph as a possible translation of the hyperterm.
  * @author tomer
  * @since 11/18/18
  */
class LocateAction(anchor: HyperTermIdentifier, goal: HyperPattern) extends Action {
  assert(anchor.identifier.kind == Programs.Kinds.NonConstructable.toString)

  /** To be used during the BFS rewrite search
    *
    * @param state the current state
    * @return is state final
    */
  protected def goalPredicate(state: RewriteSearchState): Boolean = state.graph.findEdges(anchor).nonEmpty

  override def apply(state: ActionSearchState): ActionSearchState = {
    // We assume only one root as it is a pattern from user.
    val roots = goal.edges.map(_.target) diff goal.edges.flatMap(_.sources)
    assert(roots.size == 1)
    val root = roots.head
    val destPattern = HyperGraphManyWithOrderToOne(Set(HyperEdge[TemplateTerm, TemplateTerm](root, ExplicitTerm(anchor), Seq.empty, EmptyMetadata)))

    /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
    // Create new locator rule
    val locateRule = new RewriteRule(goal, destPattern, (a, b) => EmptyMetadata)

    // Rewrite search
    val rewriteSearch = new BreadthFirstSearch[RewriteSearchState, RewriteSearchSpace]
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq :+ locateRule, initialState, goalPredicate)
    val rewriteResult = rewriteSearch.search(spaceSearch)

    // Process result
    val newEdges = rewriteResult.map(_.graph.findEdges(anchor)).toSet.flatten.take(1)
    val newProgs: Programs = Programs(state.programs.hyperGraph.addEdges(newEdges))
    if (rewriteResult.isEmpty) logger.warn("Locate did not find the requested pattern.")
    else logger.info(newProgs.reconstruct(newEdges.head.target).next().toString())
    new ActionSearchState(newProgs, state.rewriteRules)
  }
}

object LocateAction {
  case class LocateMetadata() extends Metadata {
    override def toStr: String = "LocateMetadata"
  }
}