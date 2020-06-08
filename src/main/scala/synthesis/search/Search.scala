package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait Search[S <: State[S], SS <: SearchSpace[S], R] {
  def search(searchSpace: SS): (Boolean, R)  // TODO: decide on a return value
}

trait SearchDepth[S <: State[S], SS <: SearchSpace[S], R] extends Search[S, SS, R] {
  def search(searchSpace: SS): (Boolean, R) = {
    search(searchSpace, Double.PositiveInfinity)
  }
  def search(searchSpace: SS, maxDepth: Double): (Boolean, R)
}

trait State[+This] {
  def deepCopy(): This
}

