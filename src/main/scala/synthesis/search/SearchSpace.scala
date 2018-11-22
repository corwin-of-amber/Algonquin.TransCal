package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait SearchSpace[S <: State] {
  def neighbors(state: S): Stream[S]
  def isGoal(state: S): Boolean
  def initialStates: Set[S]
}
