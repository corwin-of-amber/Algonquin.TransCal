package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import language.{Language, TranscalParser}
import syntax.AstSugar.{Term, _}
import syntax.Tree
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.GeneralizeAction.NUM_ALTS_TO_SHOW
import synthesis.{HyperTermIdentifier, Programs}

/**
  * @author tomer
  * @since 11/18/18
  */
class GeneralizeAction(anchor: HyperTermIdentifier, leaves: List[Term], name: Term) extends Action with LazyLogging {

  override def apply(state: ActionSearchState): ActionSearchState = {
    logger.debug(s"Running generalize action on $anchor")

    val roots = state.programs.hyperGraph.findEdges(anchor) map (_.target)

    // Reconstruct and generalize
    val gen =
      for (root <- roots) yield {
        (for (term <- state.programs.reconstruct(root).filter(t => leaves.diff(t.nodes).isEmpty)) yield {
          logger.debug(s"Generalizing using the term $term")
          val vars = leaves.indices map(i => TI(s"?autovar$i"))

          val functionDef: Term = {
            val fun = new Tree(name.root, vars.toList)
            new Tree(Language.letId, List(fun, term.replaceDescendants(leaves.zip(vars))))
          }

          functionDef
        }).take(NUM_ALTS_TO_SHOW)
      }

    /* select just the first generalization */
    gen.flatten.headOption match {
      case None =>
        logger.info("Failed to generalize")
        state
      case Some(newTerm) =>
        logger.info(s"Generalized to $newTerm")
        state.copy(rewriteRules = state.rewriteRules ++ new LetAction(newTerm).rules)
    }
  }
}

object GeneralizeAction {
  final val NUM_ALTS_TO_SHOW = 10
}
