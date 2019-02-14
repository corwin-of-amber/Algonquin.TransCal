package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait SearchSpace[S <: State] {
  def operators(state: S): Stream[Operator[S]]
  def neighbors(state: S): Stream[S]
  def isGoal(state: S): Boolean
  def initialStates: Set[S]
}
