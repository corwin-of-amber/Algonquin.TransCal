package synthesis.actions.operators
import synthesis.Programs
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule
import transcallang.AnnotatedTree

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
