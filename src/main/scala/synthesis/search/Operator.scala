package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait Operator[S <: State[S]] {
  def apply(state: S): S
}

trait VersionedOperator[S <: State[S]] extends Operator[S] {
  override def apply(state: S): S = apply(state, -1)._1
  def apply(state: S, lastVersion: Long): (S, Long)
}