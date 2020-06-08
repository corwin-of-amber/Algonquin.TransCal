package synthesis.search.actions

import com.typesafe.scalalogging.LazyLogging
import synthesis.search.{ActionSearchState, Operator, RewriteSearchState}

/**
  * @author tomer
  * @since 11/18/18
  */
trait Action extends Operator[ActionSearchState] with LazyLogging
trait SearchAction extends Action {
  def fromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]]): RewriteSearchState
}
