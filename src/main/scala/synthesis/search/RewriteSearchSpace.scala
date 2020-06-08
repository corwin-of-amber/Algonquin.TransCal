package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteSearchSpace(rewriteRules: Seq[Operator[RewriteSearchState]], initialState: RewriteSearchState, goalPredicate: RewriteSearchState => Boolean) extends SearchSpace[RewriteSearchState] {
  override def neighbors(state: RewriteSearchState): Stream[RewriteSearchState] =
    rewriteRules.toStream.map(rewriteRule => rewriteRule.apply(state))

  override def isGoal(state: RewriteSearchState): Boolean = goalPredicate(state)

  override def initialStates: Set[RewriteSearchState] = Set(initialState)

  override def operators(state: RewriteSearchState): Stream[Operator[RewriteSearchState]] = rewriteRules.toStream
}
