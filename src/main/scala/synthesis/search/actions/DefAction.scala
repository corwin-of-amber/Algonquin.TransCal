package synthesis.search.actions

import synthesis.search.ActionSearchState
import transcallang.{AnnotatedTree, Language}

class DefAction(term: AnnotatedTree, val allowExistential: Boolean = true, cleanTypes: Boolean = true) extends Action {
  private val letAction: LetAction = new LetAction(term, allowExistential, cleanTypes)
  private val updatedTerm = {
    val toSwitch = term.leaves.filter(_.root.literal.toString.startsWith("?"))
    val switchTo = toSwitch.map(t => AnnotatedTree.identifierOnly(t.root.copy(literal=t.root.literal.drop(1))))
    val res = term.replaceDescendants(toSwitch zip switchTo)
    if (cleanTypes) res.cleanTypes
    else res
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val isDirect = Language.builtinDirectedDefinitions.contains(term.root)
    val newProgs =
      if (isDirect) state.programs.addTerm(updatedTerm.subtrees(1))
      else state.programs.addTerm(AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, updatedTerm.subtrees))
    new ActionSearchState(newProgs, letAction(state).rewriteRules)
  }
}