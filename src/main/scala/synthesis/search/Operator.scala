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

  /** Return state after applying operator and next relevant version to run operator (should be currentVersion + 1)
    * unless operator is existential
    *
    * @param state state on which to run operator
    * @param lastVersion version from which to look for matchers in state
    * @return (new state after update, next relevant version)
    */
  def apply(state: S, lastVersion: Long): (S, Long)
}

trait StepOperator[S <: State[S]] {
  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param state current state from which to do the initial calculations and create an operator
    * @param lastVersion Version to use if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  def getStep(state: S, lastVersion: Long): VersionedOperator[S]
}