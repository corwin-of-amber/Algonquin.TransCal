package synthesis.actions.operators
import com.typesafe.scalalogging.LazyLogging
import synthesis.Programs
import synthesis.actions.ActionSearchState
import synthesis.rewrites.{RewriteSearchSpace, RewriteSearchState}
import synthesis.search.BreadthFirstSearch

/** Expands programs by running rewrite once.
  * @author tomer
  * @since 11/18/18
  */
class ExpandProgramsAction extends Action with LazyLogging {
  override def apply(state: ActionSearchState): ActionSearchState = {
    logger.trace("Expanding Programs with rewrites.")
    val rewriteSearch = new BreadthFirstSearch[RewriteSearchState, RewriteSearchSpace]
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    var counter = 0
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq, initialState, _ => {counter += 1; counter == state.rewriteRules.size})
    rewriteSearch.search(spaceSearch, 1) match {
      case None => state
      case Some(rewriteSearchState) => new ActionSearchState(new Programs(rewriteSearchState.graph), state.rewriteRules)
    }
  }
}
