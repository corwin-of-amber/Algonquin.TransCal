package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import synthesis.actions.ActionSearchState
import synthesis.search.Operator

/**
  * @author tomer
  * @since 11/18/18
  */
trait Action extends Operator[ActionSearchState] with LazyLogging