package synthesis.search

class IterativeDeepening[S <: State, SS <: SearchSpace[S]](globalMaxDepth: Double=Double.PositiveInfinity, search: SearchDepth[S, SS, S]) extends Search[S, SS, S] {
  // Search Impl.
  override def search(searchSpace: SS): Option[S] = {
    Stream.from(0, 1).takeWhile(i=>i < globalMaxDepth)  // Until we reached the max depth
      .flatMap(depth => search.search(searchSpace, depth))  // For each depth, search
      .headOption  // Find the first search which has a result
  }
}
