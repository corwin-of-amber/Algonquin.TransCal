package synthesis.actions.operators

import language.Language
import syntax.AstSugar.Term
import synthesis.actions.ActionSearchState

class DefAction(term: Term) extends Action {
  private val letAction: LetAction = new LetAction(term)

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newProgs =
      if (term.root == Language.directedLetId) state.programs.addTerm(term.subtrees(1))
      else state.programs.addTerm(term.subtrees(0)).addTerm(term.subtrees(1))
    new ActionSearchState(newProgs, letAction(state).rewriteRules)
  }
}
