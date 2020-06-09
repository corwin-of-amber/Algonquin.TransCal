package synthesis.search

import synthesis.search.actions.Action

/**
  * @author tomer
  * @since 11/18/18
  */
class ActionSearchSpace(initialState: ActionSearchState , operators: Seq[Action], goalPredicate: (ActionSearchState => Boolean)) extends SearchSpace[ActionSearchState] {
  override def neighbors(state: ActionSearchState): Stream[ActionSearchState] = {
    for(operator <- operators.toStream) yield {
      operator(state)
    }
  }

  override def initialStates: Set[ActionSearchState] = Set(initialState)

  override def operators(state: ActionSearchState): Stream[Operator[ActionSearchState]] = operators.toStream

  override def isGoal(state: ActionSearchState): Boolean = goalPredicate(state)
}
