package synthesis.search.actions

import com.typesafe.scalalogging.LazyLogging
import synthesis.search.{ActionSearchState}
import synthesis.search.rewrites.Template.ReferenceTerm
import synthesis.{HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

/**
  * @author tomer
  * @since 11/18/18
  */
class GeneralizeAction(anchor: HyperTermIdentifier, leaves: Seq[AnnotatedTree], name: AnnotatedTree, maxSearchDepth: Option[Int] = None) extends Action with LazyLogging {

  private val vars = leaves.indices map (i => AnnotatedTree.identifierOnly(Identifier(s"?autovar$i")))
  private val fun = AnnotatedTree(name.root.copy(literal=name.root.literal.replace("?", "")), vars.toList, Seq.empty)

  private def getGeneralizedTerms(progs: Programs): Set[AnnotatedTree] = {
    // TODO: Filter out expressions that use context but allow constants
    // Reconstruct and generalize
    progs.queryGraph.findEdges(anchor) map (_.target) flatMap { root =>
      (for (term <- progs.reconstruct(root).filter(t => leaves.diff(t.nodes).isEmpty)) yield {
        logger.debug(s"Generalizing using the term $term")

        AnnotatedTree(Language.letId, List(fun, term.replaceDescendants(leaves.zip(vars))), Seq.empty)
      }).take(GeneralizeAction.NUM_ALTS_TO_SHOW)
    }
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    logger.debug(s"Running generalize action on $anchor")

    val (gen, tempState): (Set[AnnotatedTree], ActionSearchState) = {
      val temp = getGeneralizedTerms(state.programs)
      if (temp.nonEmpty) {
        (temp, state)
      } else {
        logger.info("Generalize couldn't find term, trying to elaborate")
        val leavesPattern = Programs.destructPatterns(leaves, mergeRoots = false).reduce((g1, g2) => g1.++(g2.edges))
        val temp = new ElaborateAction(anchor, leavesPattern, ReferenceTerm(-1), maxSearchDepth = maxSearchDepth)(state)
        val terms = getGeneralizedTerms(temp.programs)
        if (terms.nonEmpty)
          (terms, temp)
        else {
          for (_ <- 1 to 2; op <- temp.rewriteRules) temp.updateGraph(g => op.apply(g))
          (getGeneralizedTerms(temp.programs), temp)
        }
      }
    }

    /* select just the first generalization */
    gen.headOption match {
      case None =>
        logger.info("Failed to generalize")
        state
      case Some(newTerm) =>
        logger.info(s"Generalized to ${Programs.termToString(newTerm)}")
        tempState.addRules(new LetAction(newTerm).rules)
        tempState
    }
  }
}

object GeneralizeAction {
  final val NUM_ALTS_TO_SHOW = 1
}
