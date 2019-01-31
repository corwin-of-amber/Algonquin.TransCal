package synthesis.search

import com.typesafe.scalalogging.LazyLogging

class IterativeDeepening[S <: State, SS <: SearchSpace[S]](search: SearchDepth[S, SS, S], globalMaxDepth: Double=Double.PositiveInfinity) extends Search[S, SS, S] with LazyLogging {
  // Search Impl.
  override def search(searchSpace: SS): Option[S] = {
    Stream.from(0, 1).takeWhile(i=>i < globalMaxDepth)  // Until we reached the max depth
      .flatMap(depth => {
      // For each depth, search
      logger.trace(s"Iterative deepening working on depth $depth")
      search.search(searchSpace, depth)
    }).headOption  // Find the first search which has a result
  }
}
