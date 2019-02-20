package synthesis.search

import com.typesafe.scalalogging.LazyLogging
import synthesis.rewrites.RewriteSearchState

/**
  * BFS returns last state only.
  */
class NaiveSearch[S <: State, SS <: SearchSpace[S]] extends SearchDepth[S, SS, S] with LazyLogging {

  /* --- Search Impl. --- */

  def search(searchSpace: SS, maxDepth: Double): Option[S] = {
    var state = searchSpace.initialStates.head
    var i = 0

    while (i < maxDepth && !searchSpace.isGoal(state)) {
      state = searchSpace.operators(state).drop(i % searchSpace.operators(state).size).head(state)
      i += 1
      if (i % searchSpace.operators(state).size == 0 && state.isInstanceOf[RewriteSearchState])
        logger.debug(s"Done a round robin. Graph size is: ${state.asInstanceOf[RewriteSearchState].graph.size}")
    }

    Some(state).filter(searchSpace.isGoal)
  }
}


