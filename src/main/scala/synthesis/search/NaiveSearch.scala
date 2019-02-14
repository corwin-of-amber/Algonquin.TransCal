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
      val neighbors = searchSpace.neighbors(state).map(s=>{
        logger.trace(f"In the stream!!! $i")
        s
      })
      state = neighbors.drop(i % neighbors.size).head
      i += 1
    }

    Some(state).filter(searchSpace.isGoal)
  }
}


