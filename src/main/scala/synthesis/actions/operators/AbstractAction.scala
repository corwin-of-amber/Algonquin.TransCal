package synthesis.actions.operators

import synthesis.actions.ActionSearchState
import synthesis.rewrites.{RewriteSearchSpace, RewriteSearchState}
import synthesis.search.BreadthFirstSearch

/** Abstract action.
  * @author tomer
  * @since 11/18/18
  */
abstract class AbstractAction[R] extends Action {


  /* --- Action Impl. --- */

  override def apply(state: ActionSearchState): ActionSearchState = {
    val rewriteSearch = new BreadthFirstSearch[RewriteSearchState, RewriteSearchSpace]
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq, initialState, null)
    innerApplyWithRewriter(state, rewriteSearch.search(spaceSearch))
  }


  /* --- Protected --- */

  /** Apply after rewriter results.
    * @param state The current state
    * @param rewriterResult The last rewrites result
    * @return The next action state
    */
  protected def innerApplyWithRewriter(state: ActionSearchState, rewriterResult: Option[RewriteSearchState]): ActionSearchState

}
