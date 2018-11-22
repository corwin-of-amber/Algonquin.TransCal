package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait Search[S <: State, SS <: SearchSpace[S], R] {
  def search(searchSpace: SS): Option[R]  // TODO: decide on a return value
}
