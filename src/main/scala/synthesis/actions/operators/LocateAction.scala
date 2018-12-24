package synthesis.actions.operators

import structures.HyperGraphManyWithOrderToOne
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge
import synthesis.HyperTermIdentifier
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.{Category, HyperPattern}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.rewrites.{RewriteRule, RewriteSearchState}

/** Finding a hyperterm.
  * @author tomer
  * @since 11/18/18
  */
class LocateAction(anchor: HyperTermIdentifier, goal: HyperPattern) extends AbstractAction {
  /** Apply after rewriter results.
    *
    * @param state          The current state
    * @param rewriterResult The last rewrites result
    * @return The next action state
    */
  override protected def innerApplyWithRewriter(state: ActionSearchState, rewriterResult: Option[RewriteSearchState]): ActionSearchState =

  /** To be used during the BFS rewrite search
    *
    * @param state the current state
    * @return is state final
    */
  override protected def goalPredicate(state: RewriteSearchState): Boolean = state.graph.findEdges(anchor).nonEmpty

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newHole: Int = goal.edgeTypes.map({
      case ReferenceTerm(x) => x
      case ExplicitTerm(x) => -1
    }).max + 1
    val destPattern = HyperGraphManyWithOrderToOne[TemplateTerm, TemplateTerm](HyperEdge[TemplateTerm, TemplateTerm](ReferenceTerm(newHole), ExplicitTerm(anchor), Seq.empty))
    val locateRule = RewriteRule(goal, destPattern, Category.Locator)
    super.apply(new ActionSearchState(state.programs, state.rewriteRules + locateRule))
  }
}