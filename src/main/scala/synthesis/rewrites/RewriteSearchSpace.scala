package synthesis.rewrites

import synthesis.search.SearchSpace

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteSearchSpace(rewriteRules: Seq[RewriteRule], initialState: RewriteSearchState, goalPredicate: RewriteSearchState => Boolean) extends SearchSpace[RewriteSearchState] {
  override def neighbors(state: RewriteSearchState): Stream[RewriteSearchState] =
    rewriteRules.toStream.map(rewriteRule => rewriteRule.apply(state))

  override def isGoal(state: RewriteSearchState): Boolean = goalPredicate(state)

  override def initialStates: Set[RewriteSearchState] = Set(initialState)
}
