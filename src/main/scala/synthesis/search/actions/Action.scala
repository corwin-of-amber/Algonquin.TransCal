package synthesis.search.actions

import com.typesafe.scalalogging.LazyLogging
import synthesis.search.{ActionSearchState, Operator}

/**
  * @author tomer
  * @since 11/18/18
  */
trait Action extends Operator[ActionSearchState] with LazyLogging
trait SearchAction extends Action {
  def apply(actionSearchState: ActionSearchState, depth: Option[Double]): ActionSearchState

  override def apply(state: ActionSearchState): ActionSearchState = apply(state, None)
}
