package synthesis.search.action.operators

import com.typesafe.scalalogging.LazyLogging
import synthesis.search.Operator
import synthesis.search.action.ActionSearchState
import synthesis.search.rewrite.RewriteSearchState

/**
  * @author tomer
  * @since 11/18/18
  */
trait Action extends Operator[ActionSearchState] with LazyLogging
trait SearchAction extends Action {
  def fromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]]): RewriteSearchState
}
