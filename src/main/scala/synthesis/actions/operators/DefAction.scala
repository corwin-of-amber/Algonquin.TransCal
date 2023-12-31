package synthesis.actions.operators

import transcallang.{AnnotatedTree, Language}
import synthesis.actions.ActionSearchState

class DefAction(term: AnnotatedTree) extends Action {
  private val letAction: LetAction = new LetAction(term)
  private val updatedTerm = {
    val toSwitch = term.leaves.filter(_.root.literal.toString.startsWith("?"))
    val switchTo = toSwitch.map(t => AnnotatedTree.identifierOnly(t.root.copy(literal=t.root.literal.drop(1))))
    term.replaceDescendants(toSwitch zip switchTo)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val isDirect = Language.builtinDirectedDefinitions.contains(term.root)
    val newProgs =
      if (isDirect) state.programs.addTerm(updatedTerm.subtrees(1))
      else state.programs.addTerm(AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, updatedTerm.subtrees))
    ActionSearchState(newProgs, letAction(state).rewriteRules)
  }
}
