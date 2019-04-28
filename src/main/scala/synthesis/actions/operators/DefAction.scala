package synthesis.actions.operators

import transcallang.{Identifier, Language}
import syntax.{Tree}
import synthesis.actions.ActionSearchState

class DefAction(term: Tree[Identifier]) extends Action {
  private val letAction: LetAction = new LetAction(term)
  private val updatedTerm = {
    val toSwitch = term.leaves.filter(_.root.literal.toString.startsWith("?"))
    val switchTo = toSwitch.map(t => new Tree(t.root.copy(literal=t.root.literal.drop(1))))
    term.replaceDescendants(toSwitch zip switchTo toList)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val newProgs =
      if (term.root == Language.directedLetId) state.programs.addTerm(updatedTerm.subtrees(1))
      else state.programs.addTerm(updatedTerm.subtrees(0)).addTerm(updatedTerm.subtrees(1))
    ActionSearchState(newProgs, letAction(state).rewriteRules)
  }
}
