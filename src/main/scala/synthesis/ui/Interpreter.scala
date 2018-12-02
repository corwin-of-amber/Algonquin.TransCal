package synthesis.ui

import java.io.{BufferedReader, PrintStream}

import structures.mutable.VocabularyHyperGraph
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.UserAction
import synthesis.{HyperTerm, HyperTermIdentifier, Programs}

/**
  * @author tomer
  * @since 11/24/18
  */
class Interpreter(userInput: BufferedReader, userOutput: PrintStream) {
  private val actions = Seq(new UserAction(userInput, userOutput))

  def start: Unit = {
    var newState = new ActionSearchState(new Programs(new VocabularyHyperGraph[HyperTerm, HyperTermIdentifier]()), Set())
    do {
      val oldState = newState
      for(action <- actions) {
        newState = action.apply(newState)
      }
    } while(newState != newState)
  }
}
