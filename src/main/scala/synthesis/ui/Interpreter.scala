package synthesis.ui

import java.io.{BufferedReader, PrintStream}

import syntax.AstSugar.Term
import language.Parser
import synthesis.Programs
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.UserAction

/**
  * @author tomer
  * @since 11/24/18
  */
class Interpreter(userInput: BufferedReader, userOutput: PrintStream, parser: Parser[Term]) {
  private val actions = Seq(new UserAction(userInput, userOutput, parser))

  def start: Unit = {
    var oldState: ActionSearchState = null
    var newState = ActionSearchState(Programs.empty, Set())
    do {
      oldState = newState
      for(action <- actions) {
        newState = action.apply(newState)
      }
    } while(oldState != newState)
  }
}
