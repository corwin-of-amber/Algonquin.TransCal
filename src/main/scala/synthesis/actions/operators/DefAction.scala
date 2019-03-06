package synthesis.actions.operators

import transcallang.Language
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import synthesis.actions.ActionSearchState

class DefAction(term: Term) extends Action {
  private val letAction: LetAction = new LetAction(term)
  private val updatedTerm = {
    val toSwitch = term.leaves.filter(_.root.literal.toString.startsWith("?"))
    val switchTo = toSwitch.map(t => new Tree(new Identifier(t.root.literal.toString.drop(1), t.root.kind, t.root.ns)))
    term.replaceDescendants(toSwitch zip switchTo toList)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newProgs =
      if (term.root == Language.directedLetId) state.programs.addTerm(updatedTerm.subtrees(1))
      else state.programs.addTerm(updatedTerm.subtrees(0)).addTerm(updatedTerm.subtrees(1))
    ActionSearchState(newProgs, letAction(state).rewriteRules)
  }
}
