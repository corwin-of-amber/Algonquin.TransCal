package synthesis.actions.operators

import java.io.{BufferedReader, PrintStream}

import syntax.AstSugar.{I, Term}
import language.{Language, Parser}
import structures.immutable.HyperGraphManyWithOrderToOne
import structures.{EmptyMetadata, HyperEdge, HyperGraphManyWithOrderToOneLike}
import syntax.Tree
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.rewrites.{RewriteRule, Template}

import scala.collection.mutable

/** Getting actions from user.
  *
  * @author tomer
  * @since 11/18/18
  */
class UserAction(in: Iterator[Term], out: PrintStream) extends Action {

  private def isSymbol(t: Term): Boolean = t.subtrees.isEmpty && t.root.literal != "_"

  /* --- Public --- */

  override def apply(state: ActionSearchState): ActionSearchState = {
    val baseTerm = in.next()
    logger.info("-----------------------------------------")
    logger.info(s"Received $baseTerm from user")
    val (term, annotation) = if (baseTerm.root == Language.annotationId) (baseTerm.subtrees(0), Some(baseTerm.subtrees(1)))
    else (baseTerm, None)

    val newState = term.root match {
      case Language.letId | Language.directedLetId =>
        // operator = in the main is Let (adding a new hyperterm)
        logger.info(s"Found =, adding term $term")
        annotation match {
          case Some(anno) if anno.root.literal.toString.contains("++") => new DefAction(term).apply(state)
          case _ => new LetAction(term).apply(state)
        }
      case Language.tacticId =>
        // operator ->:
        // We have 2 patterns which might hold common holes so we destruct them together
        val (lhs, rhs) = {
          val temp = Programs.destructPatternsWithRoots(term.subtrees)
          (temp.head, temp.last)
        }
        //   For left is a pattern - Locate (locating a pattern) and adding an anchor. The pattern is found using associative rules only.
        val anchor: HyperTermIdentifier = LocateAction.createTemporaryAnchor()
        logger.info(s"Locate Action: ${term.subtrees.head} with temporary anchor $anchor")
        val tempState = new LocateAction(anchor, lhs._1).apply(ActionSearchState(state.programs, AssociativeRewriteRulesDB.rewriteRules)).copy(rewriteRules = state.rewriteRules)
        val foundId = tempState.programs.hyperGraph.findEdges(anchor).headOption.map(_.target)
        val terms = {
          if (foundId.nonEmpty) {
            val res = tempState.programs.reconstructWithPattern(foundId.get, lhs._1)
            if (res.hasNext) res
            else tempState.programs.reconstruct(foundId.get)
          } else Iterator(new Tree(I("Failed")))
        }
        logger.info(s"Found: ${if (terms.hasNext) terms.next() else if(isSymbol(term.subtrees.head)) "Symbol"}")

        //   The right is:
        //   1) a symbol
        //   2) pattern => Locate and print reconstruct matching pattern
        //   3) a term => extract the left to to match the term (Generalize or extract methods)
        if (foundId.nonEmpty) term.subtrees(1) match {
          case t: Term if isSymbol(t) =>
            // A symbol - We want to add an anchor with the right name to the graph
            // t.root is the anchor from the user
            logger.info("Found locate to symbol. Adding symbol to graph.")
            new LocateAction(HyperTermIdentifier(t.root), HyperGraphManyWithOrderToOne(
              Seq(HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
                ExplicitTerm(foundId.get), ExplicitTerm(anchor), Seq.empty, EmptyMetadata)
              ): _*)).apply(tempState)
          case t: Term if t.root.literal.toString.startsWith("?") =>
            // A term to generalize - run generalize Action as is
            logger.info("Found locate to term. Running generalize.")
            new GeneralizeAction(anchor, t.subtrees, new Tree(t.root)).apply(tempState)
          case t: Term =>
            // Pattern - We want to elaborate what we found earlier into the new pattern.
            logger.info("Found locate to pattern. Running Elaborate.")
            new ElaborateAction(anchor, rhs._1, rhs._2).apply(tempState)
        }
        else {
          logger.warn("Didn't find left hand side pattern")
          state
        }
      case Language.commandId =>
        // TODO: implement operators
        // term.subtrees(0) match {
        // operator →: push stack
        // operator ←: pop stack
        // operator □: save state ?!
        // }
        state
    }

    val output: String = newState.toString
    logger.debug(s"finished processing term $baseTerm")
    logger.info("-----------------------------------------")
    logger.info("")
    newState
  }


  /* --- Privates --- */

  private val lines: mutable.Buffer[(String, ActionSearchState)] = new mutable.ListBuffer[(String, ActionSearchState)]()
}
