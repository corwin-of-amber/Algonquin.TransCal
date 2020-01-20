package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait Operator[S <: State[S]] {
  def apply(state: S): S
}

trait VersionedOperator[S <: State[S]] extends Operator[S] {
  override def apply(state: S): S

  /** Return state after applying operator and next relevant version to run operator (should be currentVersion + 1)
    * unless operator is existential
    *
    * @param state state on which to run operator
    * @return (new state after update, next relevant version)
    */
  def applyVersioned(state: S): (S)
}

trait StepOperator[A, S <: State[S]] {
  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param state current state from which to do the initial calculations and create an operator
    * @param versioned if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  def getStep(state: S, versioned: Boolean): (A)
}