package synthesis.ui

import java.io.PrintStream

import synthesis._
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.UserAction
import transcallang.AnnotatedTree

/**
  * @author tomer
  * @since 11/24/18
  */
class Interpreter(terms: Iterator[AnnotatedTree], userOutput: PrintStream) {
  private val actions = Seq(new UserAction(terms, userOutput))

  def start(): Unit = {
    var oldState: ActionSearchState = null
    var newState = ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules)
    do {
      oldState = newState
      for(action <- actions) {
        newState = action.apply(newState)
      }
    } while(terms.hasNext)
  }
}
