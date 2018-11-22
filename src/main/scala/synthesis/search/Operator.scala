package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait Operator[S <: State] {
  def apply(state: S): S
}
