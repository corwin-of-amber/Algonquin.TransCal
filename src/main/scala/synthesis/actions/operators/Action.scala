package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator

/**
  * @author tomer
  * @since 11/18/18
  */
trait Action extends Operator[ActionSearchState] with LazyLogging
trait SearchAction extends Action {
  def fromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]]): RewriteSearchState
}
