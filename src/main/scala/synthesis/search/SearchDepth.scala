package synthesis.search

trait SearchDepth[S <: State[S], SS <: SearchSpace[S], R] extends Search[S, SS, R] {
  def search(searchSpace: SS): (Boolean, R) = {
    search(searchSpace, Double.PositiveInfinity)
  }
  def search(searchSpace: SS, maxDepth: Double): (Boolean, R)
}
