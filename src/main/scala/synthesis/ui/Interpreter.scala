package synthesis.ui

import java.io.{BufferedReader, PrintStream}

import syntax.AstSugar.Term
import language.Parser
import synthesis.{Programs, SimpleRewriteRulesDB}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.UserAction

import scala.io.BufferedSource

/**
  * @author tomer
  * @since 11/24/18
  */
class Interpreter(terms: Iterator[Term], userOutput: PrintStream) {
  private val actions = Seq(new UserAction(terms, userOutput))

  def start: Unit = {
    var oldState: ActionSearchState = null
    var newState = ActionSearchState(Programs.empty, SimpleRewriteRulesDB().rewriteRules)
    do {
      oldState = newState
      for(action <- actions) {
        newState = action.apply(newState)
      }
    } while(terms.hasNext)
  }
}
