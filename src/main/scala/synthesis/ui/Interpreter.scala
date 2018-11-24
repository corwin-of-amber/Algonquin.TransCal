package synthesis.ui

import synthesis.actions.operators.UserAction

import scala.io.Source

/**
  * @author tomer
  * @since 11/24/18
  */
class Interpreter(userSource: Source) {
  val actions = Seq(new UserAction(userSource))
}
