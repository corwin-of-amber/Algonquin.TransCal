package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait Search[S <: State[S], SS <: SearchSpace[S], R] {
  def search(searchSpace: SS): (Boolean, R)  // TODO: decide on a return value
}
