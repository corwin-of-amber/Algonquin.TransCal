package synthesis.ui

import java.io.{BufferedReader, PrintStream}

import synthesis.Programs
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.UserAction

/**
  * @author tomer
  * @since 11/24/18
  */
class Interpreter(userInput: BufferedReader, userOutput: PrintStream) {
  private val actions = Seq(new UserAction(userInput, userOutput))

  def start: Unit = {
    var oldState: ActionSearchState = null
    var newState = new ActionSearchState(Programs.empty, Set())
    do {
      oldState = newState
      for(action <- actions) {
        newState = action.apply(newState)
      }
    } while(oldState != newState)
  }
}
