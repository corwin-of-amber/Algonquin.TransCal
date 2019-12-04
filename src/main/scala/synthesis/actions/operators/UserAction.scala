package synthesis.actions.operators

import java.io.PrintStream

import structures.immutable.HyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Language}

import scala.collection.mutable

/** Getting actions from user.
  *
  * @author tomer
  * @since 11/18/18
  */
class UserAction(in: Iterator[AnnotatedTree], out: PrintStream) extends Action {

  private val seperator = "---------------------------"

  private val stateStack = new mutable.ArrayStack[ActionSearchState]
  private val savedStates = new mutable.ListBuffer[ActionSearchState]

  /* --- Public --- */

  override def apply(state: ActionSearchState): ActionSearchState = {
    val baseTerm = in.next()
    logger.info(seperator)
    logger.info(s"Got ${Programs.termToString(baseTerm)} from user")
    val (tempTerm, annotation) =
      if (baseTerm.root == Language.annotationId) (baseTerm.subtrees(0), Some(baseTerm.subtrees(1)))
      else (baseTerm, None)

    val term =
      if(annotation.exists(_.root.literal.toLowerCase.startsWith("notype"))) tempTerm.map(_.copy(annotation=None))
      else tempTerm

    val newState = term.root match {
      case i if Language.builtinDefinitions.contains(i) =>
        // operator = in the main is Let (adding a new hyperterm)
        logger.info(s"Adding term ${Programs.termToString(term)} as rewrite")
        val cleanTypes = annotation.exists(a => a.root.literal.contains("typedlet"))
        annotation match {
          case Some(anno) if anno.root.literal.toString.contains("++") => new DefAction(term, cleanTypes = cleanTypes).apply(state)
          case _ => new LetAction(term, cleanTypes = cleanTypes).apply(state)
        }
      case Language.tacticId =>
        val lim = annotation.map(_.root.literal.toString).filter(_.startsWith("lim")).map(s => "lim\\(([0-9]+)\\)".r.findFirstMatchIn(s).get.group(1).toInt * state.rewriteRules.size)

        // operator ->:
        // We have 2 patterns which might hold common holes so we destruct them together
        val (lhs, rhs) = {
          val temp = Programs.destructPatternsWithRoots(Seq(term.subtrees.head.map(_.copy(annotation = None)), term.subtrees.last))
          (temp.head, temp.last)
        }
        //   For left is a pattern - Locate (locating a pattern) and adding an anchor. The pattern is found using associative rules only.
        val anchor: HyperTermIdentifier = LocateAction.createTemporaryAnchor()
        logger.info(s"LHS is Locate with pattern ${Programs.termToString(term.subtrees.head)} and temporary anchor ${anchor.identifier.literal}")
        val tempState = new LocateAction(anchor, lhs._1, maxSearchDepth = lim).apply(ActionSearchState(state.programs, AssociativeRewriteRulesDB.rewriteRules)).copy(rewriteRules = state.rewriteRules)
        val foundId = tempState.programs.hyperGraph.findEdges(anchor).headOption.map(_.target)
        val terms = {
          if (foundId.nonEmpty) {
            val res = tempState.programs.reconstructWithPattern(foundId.get, lhs._1, Some(lhs._2))
            if (res.nonEmpty) res
            else tempState.programs.reconstruct(foundId.get)
          }
          else Stream.empty
        }
        logger.info(s"Found: ${if (terms.nonEmpty) Programs.termToString(terms.head) else "failed"}")

        //   The right is:
        //   1) a symbol
        //   2) pattern => Locate and print reconstruct matching pattern
        //   3) a term => extract the left to to match the term (Generalize or extract methods)
        if (foundId.nonEmpty) term.subtrees(1) match {
          case t: AnnotatedTree if t.subtrees.isEmpty && t.root.literal != "_" =>
            // A symbol - We want to add an anchor with the right name to the graph
            // t.root is the anchor from the user
            logger.info("RHS is a symbol adding it to graph")
            val res = new LocateAction(HyperTermIdentifier(t.root), HyperGraph(
              Seq(HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
                ReferenceTerm(0), ExplicitTerm(anchor), Seq.empty, EmptyMetadata)
              ): _*), maxSearchDepth = lim).apply(tempState)
            logger.debug("Finished adding symbol.")
            res
          case t: AnnotatedTree if t.root.literal.toString.startsWith("?") =>
            // A term to generalize - run generalize Action as is
            logger.info("RHS is a term running generalize.")
            new GeneralizeAction(anchor, t.subtrees, AnnotatedTree.identifierOnly(t.root), lim).apply(tempState)
          case _: AnnotatedTree =>
            // Pattern - We want to elaborate what we found earlier into the new pattern.
            logger.info("RHS is a pattern running elaborate.")
            new ElaborateAction(anchor, rhs._1, rhs._2, lim).apply(tempState)
        }
        else {
          logger.warn("Didn't find left hand side pattern")
          state
        }
      case Language.commandId =>
        logger.info(s"Received command $term")
        term.subtrees(0).root.literal match {
          case "[]" =>
            logger.info("Saving state")
            savedStates += state
            state
          case "<-" =>
            logger.info("Adding state to stack")
            stateStack.push(state)
            state
          case "->" =>
            logger.info("Popping state from stack")
            stateStack.pop()
        }
      case Language.spbeId =>
        // should be tuples
        val typeBuilders = term.subtrees(0).subtrees.toSet
        val grammar = term.subtrees(1).subtrees.toSet
        val examples = term.subtrees(2).subtrees.map(t => t.subtrees(0) -> t.subtrees(1).subtrees).toMap
        val equivDepthOption = if (term.subtrees.length > 3) Some(term.subtrees(3).root.literal.toInt) else None
        val preRunDepth = if (term.subtrees.length > 4) Some(term.subtrees(4).root.literal.toInt) else None
        new SPBEAction(typeBuilders, grammar, examples, equivDepthOption = equivDepthOption, preRunDepth = preRunDepth)(state)
    }

    logger.info(seperator)
    logger.info("")
    newState
  }


  /* --- Privates --- */

}
