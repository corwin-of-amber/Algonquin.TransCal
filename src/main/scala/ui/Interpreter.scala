package ui

import java.io.PrintStream

import synthesis._
import synthesis.search.ActionSearchState
import synthesis.search.actions.UserAction
import synthesis.search.rewrites.{AssociativeRewriteRulesDB, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.AnnotatedTree

/**
  * @author tomer
  * @since 11/24/18
  */
class Interpreter(terms: Iterator[AnnotatedTree], userOutput: PrintStream) {
  private val actions = Seq(new UserAction(terms, userOutput))

  def start(): ActionSearchState = {
    var oldState: ActionSearchState = null
    var newState = ActionSearchState(Programs.empty, Interpreter.InterpreterRewriteRulesDB)
    do {
      oldState = newState
      for(action <- actions) {
        newState = action.apply(newState)
      }
    } while(terms.hasNext)
    newState
  }
}
object Interpreter {
  val InterpreterRewriteRulesDB = SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules
}