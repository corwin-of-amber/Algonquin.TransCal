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

  def start(): ActionSearchState = {
    var oldState: ActionSearchState = null
    var newState = ActionSearchState(Programs.empty, SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ TimeComplexRewriteRulesDB.rewriteRules ++ SpaceComplexRewriteRulesDB.rewriteRules)
    do {
      oldState = newState
      for(action <- actions) {
        newState = action.apply(newState)
      }
    } while(terms.hasNext)
    newState
  }
}
