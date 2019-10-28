package synthesis.actions.operators
import synthesis.Programs
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule
import transcallang.AnnotatedTree

/** Adds information on a called function/lambda.
  * Doing that by creating a new lambda which only call to the original function.
  *
  * @param searchTerm The term to replace.
  * @param functionOp The function header.
  * @param newPreds The new information.
  */
class SpecializeAction(searchTerm: AnnotatedTree, functionOp: AnnotatedTree, newPreds: AnnotatedTree) extends Action {

  /* --- Privates --- */

  private val Seq(
    (searchPattern, searchRoot),
    (functionOpPattern, functionOpRoot),
    (newPredsPattern, newPredsRoot)
  ) = Programs.destructPatternsWithRoots(Seq(searchTerm, functionOp, newPreds))

  /* --- Action Impl. --- */

  override def apply(state: ActionSearchState): ActionSearchState = {
    for(Seq(searchFilled, functionOpFilled, newPredFilled) <- RewriteRule.fillPatterns(state.programs.hyperGraph, Seq(searchPattern, functionOpPattern, newPredsPattern))) {
      println(searchFilled, functionOpFilled, newPredFilled)
    }
     state
  }
}
