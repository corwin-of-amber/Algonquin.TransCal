package synthesis.search.actions

import java.io.PrintStream

import structures.{EmptyMetadata, HyperEdge}
import synthesis.search.ActionSearchState
import synthesis.search.actions.thesy.{SortedVocabulary, TheoryExplorationAction}
import synthesis.search.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.rewrites.AssociativeRewriteRulesDB
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Datatype, Identifier, Language, TranscalParser}
import ui.Main
import ui.Main.splitByStatements

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
  private val knownFuncs = mutable.Map.empty[String, AnnotatedTree]
  private val knownTypes = mutable.Map.empty[Identifier, Datatype]

  private var inWithInclude = in

  /* --- Public --- */

  override def apply(state: ActionSearchState): ActionSearchState = {
    val baseTerm = inWithInclude.next()
    logger.info(seperator)
    logger.info(s"Got ${Programs.termToString(baseTerm)} from user")
    val (tempTerm, annotation) =
      if (baseTerm.root == Language.annotationId) (baseTerm.subtrees(0), Some(baseTerm.subtrees(1)))
      else (baseTerm, None)

    val term =
      if(annotation.exists(_.root.literal.toLowerCase.startsWith("notype"))) tempTerm.map(_.copy(annotation=None))
      else tempTerm

    val newState = term.root match {
      case Language.includeId =>
        inWithInclude = Main.readFile(term.subtrees.head.subtrees.head.root.literal) ++ in
        state
      case i if Language.builtinDefinitions.contains(i) =>
        // operator = in the main is Let (adding a new hyperterm)
        logger.info(s"Adding term ${Programs.termToString(term)} as rewrite")
        val cleanTypes = !annotation.exists(a => a.root.literal.contains("typedlet"))
        annotation match {
          case Some(anno) if anno.root.literal.toString.contains("++") => new DefAction(term, cleanTypes = cleanTypes).apply(state)
          case _ =>
            val let = new LetAction(term, cleanTypes = cleanTypes)
            let.apply(state)
        }
      case Language.tacticId =>
        val limFromAnno = annotation.map(_.root.literal).filter(_.startsWith("lim")).map(s => "lim\\(([0-9]+)\\)".r.findFirstMatchIn(s).get.group(1).toInt * state.rewriteRules.size)
        val lim = if (limFromAnno.isDefined) limFromAnno else Some(20)

        // operator ->:
        // We have 2 patterns which might hold common holes so we destruct them together
        val (lhs, rhs) = {
          val temp = Programs.destructPatternsWithRoots(Seq(term.subtrees.head.cleanTypes, term.subtrees.last))
          (temp.head, temp.last)
        }
        //   For left is a pattern - Locate (locating a pattern) and adding an anchor. The pattern is found using associative rules only.
        val anchor: HyperTermIdentifier = LocateAction.createTemporaryAnchor()
        logger.info(s"LHS is Locate with pattern ${Programs.termToString(term.subtrees.head)} and temporary anchor ${anchor.identifier.literal}")
        val depletedState = new ActionSearchState(state.programs, AssociativeRewriteRulesDB.rewriteRules)
        val locateState = new LocateAction(anchor, lhs._1, maxSearchDepth = lim).apply(depletedState)
        val tempState = new ActionSearchState(locateState.programs, state.rewriteRules)
        val foundId = tempState.programs.queryGraph.findEdges(anchor).headOption.map(_.target)
        val terms = {
          if (foundId.nonEmpty) {
            val res = tempState.programs.reconstructWithPattern(foundId.get, structures.mutable.HyperGraph(lhs._1.toSeq: _*), Some(lhs._2))
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
            val res = new LocateAction(HyperTermIdentifier(t.root), structures.mutable.HyperGraph(
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
            savedStates += state.deepCopy()
            state
          case "<-" =>
            logger.info("Adding state to stack")
            stateStack.push(state.deepCopy())
            state
          case "->" =>
            logger.info("Popping state from stack")
            stateStack.pop()
        }
      case Language.`thesyId` =>
        // should be tuples
        val types = term.subtrees(0).subtrees.toSet.map((t: AnnotatedTree) => knownTypes(t.root))
        val grammar = term.subtrees(1).subtrees.toSet.map((t: AnnotatedTree) => knownFuncs(t.root.literal))
        val exampleDepth = term.subtrees(2).root.literal.toInt
        val equivDepthOption = if (term.subtrees.length > 3) Some(term.subtrees(3).root.literal.toInt) else None
        val preRunDepth = if (term.subtrees.length > 4) Some(term.subtrees(4).root.literal.toInt) else None
        val splitDepth = if (term.subtrees.length > 5) Some(term.subtrees(5).root.literal.toInt) else None
        val termDepth = if (term.subtrees.length > 6) Some(term.subtrees(6).root.literal.toInt) else None
        val placeholderCount = if (term.subtrees.length > 7) Some(term.subtrees(7).root.literal.toInt) else None
        new TheoryExplorationAction(SortedVocabulary(types, grammar), exampleDepth, termDepth, equivDepthOption, preRunDepth, splitDepth, placeholderCount, false)(state)
      case Language.functionDeclId =>
        assert(term.subtrees.size == 1)
        assert(term.subtrees.head.root.annotation.nonEmpty)
        assert(term.subtrees.head.subtrees.isEmpty)
        knownFuncs(term.subtrees.head.root.literal) = term.subtrees.head
        state.updateGraph(g => g.addAllKeepVersion(Programs.destruct(term.subtrees.head)))
        state
      case Language.datatypeId =>
        assert(term.subtrees.tail.forall(_.isLeaf))
        val dt = Datatype(term.subtrees.head.root, term.subtrees.head.subtrees, term.subtrees.tail.map(_.root))
        knownTypes(term.subtrees.head.root) = Datatype(term.subtrees.head.root, term.subtrees.head.subtrees, term.subtrees.tail.map(_.root))
        dt.constructors.foreach(i => {
          knownFuncs(i.literal) = AnnotatedTree.identifierOnly(i)
        })
        state
    }

    logger.info(seperator)
    logger.info("")
    newState
  }


  /* --- Privates --- */

}
