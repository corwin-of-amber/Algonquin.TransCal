package synthesis.search.actions

import synthesis.search.ActionSearchState
import transcallang.AnnotatedTree.withoutAnnotations
import transcallang.{AnnotatedTree, Language}

/** Adds information on a called function/lambda.
  * Doing that by creating a new lambda which only call to the original function.
  *
  * @param searchTerm         The term to replace.
  * @param functionDefinition The function header and params.
  * @param newPreds           The new information, should include any additional graph information, for example (T ||| i < 3).
  */
class SpecializeAction(searchTerm: AnnotatedTree, functionDefinition: AnnotatedTree, newPreds: AnnotatedTree) extends Action {

  require(newPreds.nodes.map(_.root).forall(!_.literal.startsWith("?")))

  /* --- Privates --- */

  private val uuid = java.util.UUID.randomUUID.toString
  private val autoFuncName = "autoFunc" + uuid


  /* --- Action Impl. --- */

  override def apply(state: ActionSearchState): ActionSearchState = {
    val oldFuncName = functionDefinition.root.copy(annotation = None)
    val newFuncId = functionDefinition.root.copy(literal = autoFuncName)
    val newFuncDefinition = functionDefinition.copy(root = newFuncId)
    val specificFuncRewrites = Set(
      new LetAction(withoutAnnotations(Language.limitedDirectedLetId, Seq(newFuncDefinition, newPreds))),
      new LetAction(withoutAnnotations(Language.directedLetId, Seq(newFuncDefinition, functionDefinition))),
      new LetAction(withoutAnnotations(Language.directedLetId, Seq(searchTerm, searchTerm.map({
        case i if i == oldFuncName => newFuncId.copy(annotation = None)
        case i => i
      }))))
    ).flatMap(_.rules)

    state.addRules(specificFuncRewrites)
    state
  }
}
