package synthesis.actions.operators

import synthesis.actions.ActionSearchState

import scala.io.Source

/**
  * @author tomer
  * @since 11/18/18
  */
class UserAction(inputSource: Source) extends Action {
  override def apply(state: ActionSearchState): ActionSearchState = {
    state
  }
}
