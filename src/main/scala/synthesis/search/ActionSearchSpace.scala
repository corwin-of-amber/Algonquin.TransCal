package synthesis.search

import synthesis.search.actions.Action

/**
  * @author tomer
  * @since 11/18/18
  */
class ActionSearchSpace(initialState: ActionSearchState , operators: Seq[Action]) extends SearchSpace[ActionSearchState] {
  override def neighbors(state: ActionSearchState): Stream[ActionSearchState] = {
    for(operator <- operators.toStream) yield {
      operator(state)
    }
  }

  override def isGoal(state: ActionSearchState): Boolean = true

  override def initialStates: Set[ActionSearchState] = Set(initialState)

  override def operators(state: ActionSearchState): Stream[Operator[ActionSearchState]] = operators.toStream
}
