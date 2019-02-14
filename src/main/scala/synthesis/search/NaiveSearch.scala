package synthesis.search

import com.typesafe.scalalogging.LazyLogging

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
    }

    Some(state).filter(searchSpace.isGoal)
  }
}


