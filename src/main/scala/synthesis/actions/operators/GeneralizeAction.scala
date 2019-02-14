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
    val updatedGraph = new Programs(state.programs.hyperGraph.filter(e => e.sources.nonEmpty || leaves.map(_.root).contains(e.edgeType.identifier)))

    // Reconstruct and generalize
    val gen =
      for (root <- roots) yield {
        (for (term <- updatedGraph.reconstruct(root)) yield {
          logger.debug(s"Generalizing using the term $term")
          val vars = leaves.indices map(i => TI(s"?autovar$i"))

          val functionDef: Term = {
            val fun = new Tree(name.root, vars.toList)
            new Tree(Language.letId, List(fun, term.replaceDescendants(leaves.zip(vars))))
          }

          new LetAction(functionDef).rules
        }).take(NUM_ALTS_TO_SHOW)
      }

    /* select just the first generalization */
    gen.flatten.headOption match {
      case None => state
      case Some(newRules) =>
        state.copy(rewriteRules = state.rewriteRules ++ newRules)
    }
  }
}

object GeneralizeAction {
  final val NUM_ALTS_TO_SHOW = 10
}
