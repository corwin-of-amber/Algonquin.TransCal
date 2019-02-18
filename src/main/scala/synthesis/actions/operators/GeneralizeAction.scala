package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import language.Language
import structures.EmptyMetadata
import syntax.AstSugar.{Term, _}
import syntax.Tree
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.GeneralizeAction.NUM_ALTS_TO_SHOW
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import synthesis.{HyperEdgeTargetOrdering, HyperTermIdentifier, Programs}

/**
  * @author tomer
  * @since 11/18/18
  */
class GeneralizeAction(anchor: HyperTermIdentifier, leaves: List[Term], name: Term) extends Action with LazyLogging {

  private val vars = leaves.indices map (i => TI(s"?autovar$i"))
  private val fun = new Tree(name.root, vars.toList)

  private def getGeneralizedTerms(progs: Programs): Set[Term] = {
    // TODO: Filter out expressions that use context but allow constants
    // Reconstruct and generalize
    println(progs.hyperGraph.edgesOrdered(HyperEdgeTargetOrdering).map(_.copy(metadata = EmptyMetadata)))
    progs.hyperGraph.findEdges(anchor) map (_.target) flatMap { root =>
      (for (term <- progs.reconstruct(root).filter(t => leaves.diff(t.nodes).isEmpty)) yield {
        logger.debug(s"Generalizing using the term $term")

        new Tree(Language.letId, List(fun, term.replaceDescendants(leaves.zip(vars))))
      }).take(NUM_ALTS_TO_SHOW)
    }
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    logger.debug(s"Running generalize action on $anchor")

    val (gen, tempState): (Set[Term], ActionSearchState) ={
      val temp = getGeneralizedTerms(state.programs)
      if(temp.nonEmpty) {
        (temp, state)
      } else {
        logger.info("Generalize couldn't find term, trying to elaborate")
        val leavesPattern = Programs.destructPatterns(leaves, mergeRoots = false).reduce((g1, g2) => g1.addEdges(g2.edges))
        val temp = new ElaborateAction(anchor, leavesPattern, ReferenceTerm(-1))(state)
        var rewriteSearchState = new RewriteSearchState(temp.programs.hyperGraph)
        for(i <- 1 to 2; op <- temp.rewriteRules) {
          rewriteSearchState = op(rewriteSearchState)
        }
        val progs = new Programs(rewriteSearchState.graph)
        (getGeneralizedTerms(progs), ActionSearchState(progs, temp.rewriteRules))
      }
    }

    /* select just the first generalization */
    gen.headOption match {
      case None =>
        logger.info("Failed to generalize")
        tempState
      case Some(newTerm) =>
        logger.info(s"Generalized to $newTerm")
        tempState.copy(rewriteRules = tempState.rewriteRules ++ new LetAction(newTerm).rules)
    }
  }
}

object GeneralizeAction {
  final val NUM_ALTS_TO_SHOW = 1
}
