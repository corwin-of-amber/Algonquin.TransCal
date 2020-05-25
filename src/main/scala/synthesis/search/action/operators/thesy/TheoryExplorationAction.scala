package synthesis.search.action.operators.thesy

import report.StopWatch
import structures.{EmptyMetadata, Metadata}
import synthesis.search.Operator
import synthesis.search.action.ActionSearchState
import synthesis.search.action.ActionSearchState.HyperGraph
import synthesis.search.action.operators._
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.rewrite.operators.Template.ReferenceTerm
import synthesis.search.rewrite.operators.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Datatype, Identifier, Language, TranscalParser}

import scala.collection.mutable

// Constructors than split by datatype
// Grammar use this during graph expansion
class TheoryExplorationAction(typeBuilders: Set[Identifier],
                              grammar: Set[AnnotatedTree],
                              examples: Map[AnnotatedTree, Seq[AnnotatedTree]],
                              termDepthOption: Option[Int] = None,
                              equivDepthOption: Option[Int] = None,
                              splitDepthOption: Option[Int] = None,
                              preRunDepth: Option[Int] = None,
                              placeholderCountOption: Option[Int] = None) extends Action {
  assert(examples.values.map(_.size).toSet.size == 1)
  var termCount = 0
  var failedProofs = 0
  var retriedProofs = 0
  val allfailed: mutable.Set[(AnnotatedTree, AnnotatedTree)] = mutable.Set.empty

  val equivDepth = equivDepthOption.getOrElse(4)

  val splitDepth = splitDepthOption.getOrElse(1)
  val termDepth = termDepthOption.getOrElse(2)
  val placeholderCount = placeholderCountOption.getOrElse(2)

  // TODO: change input into sorted vocab
  private val vocab = {
    val baseType = typeBuilders.find(_.annotation.get.root != Language.mapTypeId).flatMap(_.annotation).get
    SortedVocabulary(Seq(Datatype(baseType.root, baseType.subtrees, typeBuilders.toSeq)), grammar.toSeq)
  }

  private val searcher = {
    val randomChooser = CaseSplitAction.randomChooser(equivDepth, splitDepth)
    new OperatorRunWithCaseSplit(equivDepth, splitDepth = Some(splitDepth), chooser = Some(randomChooser), preRunDepth = preRunDepth)
  }

  // TODO: fix tests and make this private
  val conjectureGenerator = new ConjectureGenerator(vocab, searcher, examples, placeholderCount)

  // How to do this? for now given by user
  // Maybe i should use the rewrite system
  // * Create sygus rules only from constructors and grammer.
  // * Create placeholders ahead of time
  // * Base graph of placeholders and constants of created type
  // * Find all hyper terms by type
  // * reconstruct

  protected def retryFailed(failedAttempts: mutable.Buffer[(AnnotatedTree, AnnotatedTree)], actionState: ActionSearchState): (Set[(AnnotatedTree, AnnotatedTree)], ActionSearchState) = {
    var i = 0
    var state = actionState
    val found = mutable.Buffer.empty[(AnnotatedTree, AnnotatedTree)]
    logger.info("Retrying failed proofs")
    while (i < failedAttempts.length) {
      val (term1, term2) = failedAttempts.head
      val anchor = LocateAction.createTemporaryAnchor()
      val (lhs, rhs) = {
        val temp = Programs.destructPatternsWithRoots(Seq(term1, term2))
        (temp.head, temp.last)
      }
      val state1 = ActionSearchState(Programs(term2), state.rewriteRules)
      val state2 = ActionSearchState(Programs(term1), state.rewriteRules)
      val newState1 = new ElaborateAction(anchor, lhs._1, lhs._2, maxSearchDepth = Some(equivDepth))(state1)
      val newState2 = new ElaborateAction(anchor, rhs._1, rhs._2, maxSearchDepth = Some(equivDepth))(state2)
      if (newState1 == state && newState2 == state) {
        failedAttempts.remove(i)
      } else {
        val res = inductionStep(state, term1, term2).collect({ case rr => rr })
        if (res.nonEmpty) {
          state = ActionSearchState(state.programs, state.rewriteRules ++ res)
          found.append((term1.map(i => if (i.literal.startsWith("Placeholder")) i.copy(literal = "?" + i.literal) else i),
            term2.map(i => if (i.literal.startsWith("Placeholder")) i.copy(literal = "?" + i.literal) else i)))
          failedAttempts.remove(i)
          retriedProofs += 1
          i = 0
        } else i += 1
      }
    }
    logger.info("Done Retrying failed proofs")
    (found.toSet, state)
  }

  private def findNewRules(actionState: ActionSearchState, rewriteStateProgs: Programs, depth: Int)
  : (Set[(AnnotatedTree, AnnotatedTree)], ActionSearchState) = {
    var state = actionState
    val entries = Programs.reconstructAll(rewriteStateProgs.hyperGraph, depth)
    termCount = math.max(entries.size, termCount)

    val failedAttempts = mutable.Buffer.empty[(AnnotatedTree, AnnotatedTree)]

    implicit val smallestGeneraliestOrdering: Ordering[AnnotatedTree] = new Ordering[AnnotatedTree] {
      def getphs(annotatedTree: AnnotatedTree) = annotatedTree.nodes.map(_.root.literal).filter(_.toLowerCase.contains("placeholder")).toSet.size

      override def compare(x: AnnotatedTree, y: AnnotatedTree): Int =
        if (x.size < y.size) -1
        else if (x.size > y.size) 1
        else if (getphs(x) > getphs(y)) -1
        else if (getphs(x) < getphs(y)) 1
        else 0
    }

    val entryGroups = entries.groupBy(_.edge.target)
    // Sort groups by smallest tree with max variables
    val res = (for ((r, terms) <- entryGroups.toSeq.sortBy(_._2.map(_.tree).min) if terms.size > 1) yield {
      // Before running induction steps, should filter out some of the terms.
      // To do that I will run an observational equivalence step and insert temporary rules.
      // Because we might need these temporary rules to help with future induction rules.
      logger.info("Filtering terms that don't need induction using observational equivalence")
      val equives = new ObservationalEquivalence(equivDepth)
        .fromTerms(terms.map(_.tree).toSeq, state.rewriteRules)
      // Take smallest of each set as heuristic to have no unneeded subterms
      val filteredTerms = equives.map(_.minBy(_.size)).toSeq
      // Do the induction step for each couple of terms
      val temp = (for ((term1, term2) <- filteredTerms.combinations(2).toSeq.map(it => (it(0), it(1)))) yield {
        val res = inductionStep(state, term1, term2).collect({ case rr => rr })
        if (res.nonEmpty) {
          state = ActionSearchState(state.programs, state.rewriteRules ++ res)
          logger.warn(s"Retrying failed depth ${depth}  @  ${StopWatch.instance.now}")
          val (newOnes, newState) = retryFailed(failedAttempts, state)
          logger.warn(s"Finished refailed depth ${depth}  @  ${StopWatch.instance.now}")
          state = newState
          if (allfailed.contains((term1, term2)))
            retriedProofs += 1
          newOnes ++ Some((term1.map(i => if (i.literal.startsWith("Placeholder")) i.copy(literal = "?" + i.literal) else i),
            term2.map(i => if (i.literal.startsWith("Placeholder")) i.copy(literal = "?" + i.literal) else i)))
        } else {
          failedAttempts.append((term1, term2))
          failedProofs += 1
          allfailed.add((term1, term2))
          allfailed.add((term2, term1))
          Set.empty[(AnnotatedTree, AnnotatedTree)]
        }
      })
      temp.flatten
    })
    (res.flatten.toSet, state)
  }

  override def apply(initialState: ActionSearchState): ActionSearchState = {
    var state = initialState
    val foundRules = mutable.Buffer.empty[mutable.Buffer[(AnnotatedTree, AnnotatedTree)]]
    var newRules = Set.empty[(AnnotatedTree, AnnotatedTree)]
    var newOps = Set.empty[Operator[RewriteSearchState]]

    for (i <- 1 to termDepth) {
      logger.warn(s"Running SPBE in depth ${i}  @  ${StopWatch.instance.now}")
      logger.info(s"Creating terms of depth $i")
      conjectureGenerator.increaseDepth()
      // ******** SPBE ********
      foundRules += mutable.Buffer.empty
      // Gives a graph of depth i+~ applications of funcs on known terminals and functions
      // Because we merge in the graph there is no need to remember equivalences already found

      logger.warn(s"Finished term creation depth ${i}  @  ${StopWatch.instance.now}")
      logger.info(s"Trying to merge terms")
      val equivalenceClasses = conjectureGenerator.inferConjectures(state.rewriteRules)
      logger.warn(s"Finished symbolic term evaluation depth ${i}  @  ${StopWatch.instance.now}")
      // Prove equivalence by induction.
      logger.info(s"Working on equivalences")
      // Different context for temp names
      //      val pattern1 = Programs.destructPattern(new TranscalParser().parseExpression("or(p1, fold or false p0)").map(i => if(i.literal == "p0") i.copy(literal="Placeholder_0_type_{list(boolean)}") else i).map(i => if(i.literal == "p1") i.copy(literal="Placeholder_0_type_{boolean}") else i))
      //      val pattern2 = Programs.destructPattern(new TranscalParser().parseExpression("fold or (or(p1, _)) p0").map(i => if(i.literal == "p0") i.copy(literal="Placeholder_0_type_{list(boolean)}") else i).map(i => if(i.literal == "p1") i.copy(literal="Placeholder_0_type_{boolean}") else i))
      findNewRules(state, Programs(equivalenceClasses.graph), i) match {
        case (rules, newstate) =>
          newRules = rules
          newOps = newstate.rewriteRules -- state.rewriteRules
          state = newstate
      }
      logger.warn(s"Finished finding rules depth ${i}  @  ${StopWatch.instance.now}")
      foundRules.last ++= newRules
      logger.info(s"Found new lemmas in depth $i:")
      for ((t1, t2) <- foundRules.last)
        logger.info(s"  ${Programs.termToString(t1)} = ${Programs.termToString(t2)}")
    }

    logger.info(s"Searching for rules that have become provable:")
    var continue = 0
    if (newRules.nonEmpty) {
      val equivalenceClasses = conjectureGenerator.inferConjectures(state.rewriteRules)
      do {
        // Reset versioning to look only on results from new rules.
        val progs = Programs(equivalenceClasses.graph)
        findNewRules(state, progs, termDepth) match {
          case (rules, newstate) =>
            newRules = rules
            newOps = newstate.rewriteRules -- state.rewriteRules
            state = newstate
        }
        continue += 1
        logger.warn(s"Finished finding rules repeat $continue depth ${termDepth}  @  ${StopWatch.instance.now}")
        for ((t1, t2) <- newRules) {
          logger.info(s"  ${Programs.termToString(t1)} == ${Programs.termToString(t2)}")
        }
        foundRules.last ++= newRules
      } while (newRules.nonEmpty)
    }
    logger.info("Done searching for rules:")
    for ((t1, t2) <- foundRules.flatten)
      logger.info(s"  ${Programs.termToString(t1)} == ${Programs.termToString(t2)}")
    //    }

    logger.info(s"Done SPBE  @  ${StopWatch.instance.now}")
    logger.info(s"term count: $termCount")
    logger.info(s"failed count: $failedProofs")
    logger.info(s"retry success count: $retriedProofs")
    state
  }

  private val ltwfId = Identifier("ltwf")
  private val ltwfTransivity = new LetAction(new TranscalParser()("ltwf(?x, ?y) ||| ltwf(?z, x) >> ltwf(z, y)")).rules
  private val ltwfByConstructor = {
    vocab.datatypes.flatMap(d => d.constructors.flatMap({ c =>
        val holesAndIgnores = c.annotation.get.subtrees.dropRight(1).zipWithIndex.map({
          // TODO: Once we support dependant types or enumerationg polymorphic types instantiantions we need to change this
          case (t, i) if t == d.asType => AnnotatedTree.identifierOnly(Identifier(s"?param$i"))
          case _ => AnnotatedTree.identifierOnly(Identifier("_"))
        })
        val rootTree = AnnotatedTree.identifierOnly(Identifier("?root"))
        val lhs = AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(
          rootTree,
          AnnotatedTree.withoutAnnotations(c.copy(annotation = None), holesAndIgnores)
        ))

        val trueTree = AnnotatedTree.identifierOnly(Language.trueId)
        val holesLtwf = AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, trueTree +: holesAndIgnores.collect({
          case holeTree if holeTree.root.literal.startsWith("?") =>
            AnnotatedTree.withoutAnnotations(ltwfId, Seq(holeTree, rootTree))
        }))

        new LetAction(AnnotatedTree.withoutAnnotations(Language.limitedDirectedLetId, Seq(
          lhs,
          holesLtwf
        ))).rules
    }))
  }
  private val ltfwRules = ltwfTransivity ++ ltwfByConstructor

  private def cleanVars(i: Identifier): Identifier =
    if (i.literal.startsWith("?")) i.copy(literal = i.literal.drop(1)) else i

  private def identMapper(i: Identifier, inductionPh: Identifier, inductionVarName: String = "?inductionVar")
  : Identifier = i match {
    case i if i == inductionPh => i.copy(literal = inductionVarName)
    case i if i.literal.startsWith("Placeholder") => i.copy(literal = "?" + i.literal)
    case i => i
  }

  private def createHypothesis(term1: AnnotatedTree,
                               term2: AnnotatedTree,
                               inductionPh: Identifier,
                               replacementVarLiteral: String = "?SomeVar"): Seq[RewriteRule] = {
    val hypothIndcVar = "?SomeVar"
    val (cleanTerm1, cleanTerm2) = (term1.map(identMapper(_, inductionPh, hypothIndcVar)),
      term2.map(identMapper(_, inductionPh, hypothIndcVar)))

    val precondition = AnnotatedTree.withoutAnnotations(ltwfId,
      Seq(Identifier("?SomeVar"), cleanVars(identMapper(inductionPh, inductionPh))).map(AnnotatedTree.identifierOnly))

    // Precondition on each direction of the hypothesis
    Seq((cleanTerm1, cleanTerm2), (cleanTerm2, cleanTerm1)).flatMap({ case (t1, t2) =>
      new LetAction(AnnotatedTree.withoutAnnotations(Language.directedLetId, Seq(
        AnnotatedTree.withoutAnnotations(Language.trueCondBuilderId, Seq(precondition, t1)), t2
      ))).rules
    })
  }

  def inductionStep(state: ActionSearchState, typedTerm1: AnnotatedTree, typedTerm2: AnnotatedTree): Set[RewriteRule] = {
    // Each placeholder represents a value of a type.
    // To deal with multi param expressions some of the placeholders were duplicated ahead of time, so now just use 'em

    val term1 = typedTerm1 cleanTypes
    val term2 = typedTerm2 cleanTypes
    // TODO: Once we support dependant types or enumerationg polymorphic types instantiantions we need to change this
    val relevantTypes = vocab.datatypes.filter(d => typedTerm1.nodes.map(_.root.annotation).contains(Some(d.asType))
      || typedTerm2.nodes.map(_.root.annotation).contains(Some(d.asType)))

    if (relevantTypes.isEmpty) return Set.empty

    assert(relevantTypes.size == 1)
    val ourType = relevantTypes.head

    // Create new rewrite rule for induction hypothesis
    // Need to demand that the used hypothesis works only on structures smaller than new applied constructor
    // For that i created the ltwf relation. The constructor rules will add the needed intial relations.
    // So I will do directed let and add ltwf(?params from correct type, PH0) on the premise
    val inductionPh = conjectureGenerator.inductionVar(ourType.asType).copy(annotation = None)
    if ((!term1.nodes.map(_.root).contains(inductionPh)) || (!term2.nodes.map(_.root).contains(inductionPh)))
      return Set.empty

    logger.info(s"Trying to prove ${Programs.termToString(term1)} == ${Programs.termToString(term2)}")

    val updatedTerm1 = term1.map(identMapper(_, inductionPh))
    val updatedTerm2 = term2.map(identMapper(_, inductionPh))

    val hypoths = createHypothesis(term1, term2, inductionPh)

    // TODO: isFunctionType should be as language utils
    val conses = ourType.constructors.filter(_.annotation.exists(_.root == Language.mapTypeId))
    if (conses.forall({ c =>
        // Replace inductionPh by inductionVar
        // Create base graph where inductionPh ||| c(existentials: _*)
        val constructedVal = AnnotatedTree.withoutAnnotations(c.copy(annotation = None),
          c.annotation.get.subtrees.dropRight(1).zipWithIndex.map({ case (_, i) =>
            AnnotatedTree.identifierOnly(Identifier(s"param$i"))
          })
        )
        val phToConstructed = new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(
          AnnotatedTree.identifierOnly(cleanVars(identMapper(inductionPh, inductionPh))),
          constructedVal
        )))

        val cleanUpdatedTerms = Seq(updatedTerm1, updatedTerm2).map(_.map(cleanVars))
        val actionState = ActionSearchState(Programs(cleanUpdatedTerms.head).addTerm(cleanUpdatedTerms.last),
          ltfwRules ++ hypoths ++ phToConstructed.rules ++ state.rewriteRules)
        val nextState = new OperatorRunWithCaseSplit(equivDepth, preRunDepth = preRunDepth)(actionState)
        val pattern = Programs.destructPattern(AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, cleanUpdatedTerms))
        nextState.programs.hyperGraph.findSubgraph[Int](pattern).nonEmpty
    })) {
      logger.info(s"Found inductive rule: ${Programs.termToString(updatedTerm1)} = ${Programs.termToString(updatedTerm2)}")
      new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(updatedTerm1, updatedTerm2)),
        allowExistential = false).rules
    } else {
      logger.info(s"Proof Failed")
      Set.empty
    }
  }
}