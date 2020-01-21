package synthesis.actions.operators

import structures.HyperEdge
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.actions.ActionSearchState.HyperGraph
import synthesis.rewrites.Template.ReferenceTerm
import synthesis.rewrites.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.{Operator, StepOperator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

import scala.collection.mutable

// Constructors than split by datatype
// Grammar use this during graph expansion
class SPBEAction(typeBuilders: Set[AnnotatedTree],
                 grammar: Set[AnnotatedTree],
                 examples: Map[AnnotatedTree, Seq[AnnotatedTree]],
                 termDepthOption: Option[Int] = None,
                 equivDepthOption: Option[Int] = None,
                 splitDepthOption: Option[Int] = None,
                 preRunDepth: Option[Int] = None) extends Action {
  assert(examples.values.map(_.size).toSet.size == 1)

  val termDepth = termDepthOption.getOrElse(2)
  val splitDepth = splitDepthOption.getOrElse(1)
  val equivDepth = equivDepthOption.getOrElse(4)

  val anchorPrefix = "SPBE_"
  // How to do this? for now given by user
  // Maybe i should use the rewrite system
  // * Create sygus rules only from constructors and grammer.
  // * Create placeholders ahead of time
  // * Base graph of placeholders and constants of created type
  // * Find all hyper terms by type
  // * reconstruct

  private def isFunctionType(annotatedTree: AnnotatedTree) = annotatedTree.root.annotation match {
    case Some(annotation) => annotation.root == Language.mapTypeId
    case None => false
  }

  def createPlaceholder(placeholderType: AnnotatedTree, i: Int): Identifier =
    Identifier(literal = s"Placeholder_${i}_type_{${Programs.termToString(placeholderType)}}",
      annotation = Some(placeholderType))

  private val randomChooser = CaseSplitAction.randomChooser(equivDepth, splitDepth)

  //  private val constants = typeBuilders.filterNot(isFunctionType) ++ grammar.filterNot(isFunctionType)
  private val constants = grammar.filterNot(isFunctionType)

  private val constructors = typeBuilders.filter(isFunctionType)

  private val sygusRules = SyGuSRewriteRules(
    //    constructors ++
    grammar.filter(isFunctionType).map(t => t.copy(subtrees = Seq.empty))
  ).rewriteRules.map(_.asInstanceOf[RewriteRule])

  private val types: Set[AnnotatedTree] =
    (constructors ++ grammar.filter(isFunctionType).map(t => t.copy(subtrees = Seq.empty)))
      .flatMap(_.root.annotation.get match {
        case AnnotatedTree(Language.mapTypeId, trees, _) => trees
        case _ => throw new RuntimeException("All constructors should be function types")
      })

  private val placeholders: Map[AnnotatedTree, Seq[Identifier]] =
    types.map(t => (t, 0 to 2 map (i => createPlaceholder(t, i)))).toMap

  private def createBaseGraph(typed: Boolean): HyperGraph = {
    val symbols = placeholders.values.flatMap(ps => ps.map(AnnotatedTree.identifierOnly)).toSeq ++ grammar
    // Trying to go back to using tuples
    //    val addSplits = examples.map({ case (typ, exs) =>
    //      AnnotatedTree.withoutAnnotations(CaseSplitAction.possibleSplitId,
    //        AnnotatedTree.identifierOnly(placeholders(typ).head) +: exs)
    //    })
    val addSplits = Seq.empty
    val symbolsToUse = symbols.map(t => AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(
      AnnotatedTree.identifierOnly(Language.trueId),
      AnnotatedTree.withoutAnnotations(SyGuSRewriteRules.sygusCreatedId, Seq(t))
    )))
    val tree = if (symbols.size > 1)
      AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, symbolsToUse ++ addSplits)
    else
      symbolsToUse.head
    val res =
      if (typed) Programs.destruct(tree)
      else Programs.destruct(tree.map(_.copy(annotation = None)))
    res
  }

  val baseGraph: ActionSearchState.HyperGraph = createBaseGraph(true)

  val untypedBaseGraph: ActionSearchState.HyperGraph = createBaseGraph(false)

  private def findNewRules(actionState: ActionSearchState, rewriteStateProgs: Programs, depth: Int)
  : (Set[(AnnotatedTree, AnnotatedTree)], ActionSearchState) = {
    var state = actionState
    val entries = Programs.reconstructAll(rewriteStateProgs.hyperGraph, depth)

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
      (for ((term1, term2) <- filteredTerms.combinations(2).toSeq.map(it => (it(0), it(1)))) yield {
        val res = inductionStep(state, term1, term2).collect({ case rr => rr })
        if (res.nonEmpty) {
          state = ActionSearchState(state.programs, state.rewriteRules ++ res)
          Some((term1.map(i => if (i.literal.startsWith("Placeholder")) i.copy(literal = "?" + i.literal) else i),
            term2.map(i => if (i.literal.startsWith("Placeholder")) i.copy(literal = "?" + i.literal) else i)))
        } else None
      }).collect({ case Some(x) => x })
    })
    (res.flatten.toSet, state)
  }

  override def apply(initialState: ActionSearchState): ActionSearchState = {
    var rewriteState = new RewriteSearchState(baseGraph)
    var state = initialState
    val foundRules = mutable.Buffer.empty[mutable.Buffer[(AnnotatedTree, AnnotatedTree)]]
    var newRules = Set.empty[(AnnotatedTree, AnnotatedTree)]

    logger.info("Running SPBE")
    for (i <- 1 to termDepth) {
      logger.info(s"Creating terms of depth $i")
      // ******** SPBE ********
      foundRules += mutable.Buffer.empty
      // Gives a graph of depth i+~ applications of funcs on known terminals and functions
      // Because we merge in the graph there is no need to remember equivalences already found
      rewriteState = sygusStep(rewriteState)
      logger.info(s"Trying to merge terms")
      rewriteState = findAndMergeEquives(rewriteState, state.rewriteRules.toSeq)
      // Prove equivalence by induction.
      logger.info(s"Working on equivalences")
      // Different context for temp names
      findNewRules(state, Programs(rewriteState.graph), i) match {
        case (rules, newstate) => newRules = rules; state = newstate
      }
      foundRules.last ++= newRules
      logger.info(s"Found new lemmas in depth $i:")
      for ((t1, t2) <- foundRules.last)
        logger.info(s"  ${Programs.termToString(t1)} = ${Programs.termToString(t2)}")
    }

    logger.info(s"Searching for rules that became proovable:")
    var continue = 2
    if (newRules.nonEmpty) {
      while (continue > 0) {
        continue -= 1
        do {
          val progs = Programs(rewriteState.graph)
          findNewRules(state, progs, termDepth) match {
            case (rules, newstate) => newRules = rules; state = newstate
          }
          for ((t1, t2) <- newRules)
            logger.info(s"  ${Programs.termToString(t1)} == ${Programs.termToString(t2)}")
          if (newRules.nonEmpty) continue = 1
        } while (newRules.nonEmpty)
        if (continue > 0)
          rewriteState = findAndMergeEquives(rewriteState, state.rewriteRules.toSeq)
      }
    }

    state
  }

  def createTupeledGraph(rewriteSearchState: RewriteSearchState) = {
    val exampleLength = examples.values.head.length
    // Need to shift all edges on graph because we want to prevent one changed value from affecting parts of other
    // values. Working on edges to prevent unwanted compaction until adding back anchors and adding new tuples.
    val replacedGraphs = (0 until exampleLength).map(i => {
      val itAnchorPrefix = s"${i}_$anchorPrefix"
      //      val currentGraph = rewriteSearchState.graph.clone.filterNot(e => e.edgeType.identifier == Language.typeId
      //        || e.edgeType.identifier == SyGuSRewriteRules.sygusCreatedId)
      val currentGraph = rewriteSearchState.graph.clone
      // DO NOT CHANGE GRAPH BEFORE ADDING ANCHORS. SEE getTupledConclusions
      currentGraph ++= currentGraph.edges.map(e => ObservationalEquivalence.createAnchor(itAnchorPrefix, e.target))
      val currentExamples = examples.map(t => (t._1, t._2(i)))
      currentExamples.foreach({ case (typ, example) =>
        val ph = createPlaceholder(typ, 0).copy(annotation = None)
        currentGraph ++= Programs.destruct(example.map(_.copy(annotation = None)), maxId = currentGraph.nodes.maxBy(_.id))
        val (pattern, root) = Programs.destructPatternsWithRoots(Seq(example.map(_.copy(annotation = None)))).head
        val id = currentGraph.findSubgraph[Int](pattern).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
        currentGraph.mergeNodesInPlace(currentGraph.findByEdgeType(HyperTermIdentifier(ph)).head.target, id)
        currentGraph --= currentGraph.findByEdgeType(HyperTermIdentifier(ph))
      })
      currentGraph
    })
    val resGraph = replacedGraphs.head
    var max = resGraph.nodes.maxBy(_.id)
    replacedGraphs.tail.foreach(g => {
      resGraph ++= CaseSplitAction.shiftEdges(max.id + 1, g.edges)
      max = resGraph.nodes.maxBy(_.id)
    })
    new RewriteSearchState(resGraph)
  }

  def getTupledConclusions(rewriteSearchState: RewriteSearchState): Set[Set[HyperTermId]] = {
    val prefixes = examples.values.head.indices.map(i => s"${i}_$anchorPrefix")
    // This can work because when we create the tupled graph we do not change the graph when inserting different anchors
    ObservationalEquivalence.flattenIntersectConclusions(prefixes.map(p =>
      ObservationalEquivalence.getIdsToMerge(p, rewriteSearchState.graph.edges))).filter(_.nonEmpty)
  }

  def sygusStep(state: RewriteSearchState): RewriteSearchState = {
    val hyperTermIds: Seq[() => HyperTermId] = 0 until sygusRules.size map (i => {
      val creator = Stream.from(if (state.graph.isEmpty) i else state.graph.nodes.map(_.id).max + 1 + i, sygusRules.size).map(HyperTermId).iterator
      () => creator.next
    })

    val res = sygusRules.par.map((r: RewriteRule) => r.getStep(state, false))
    val newEdges = res.zip(hyperTermIds).map({ case (es, idCreator) => structures.generic.HyperGraph.fillWithNewHoles(es, idCreator) }).seq.flatten
    logger.debug(s"Found ${newEdges.size} new edges using sygus")
    state.graph ++= newEdges
    FunctionArgumentsAndReturnTypeRewrite(state)
  }

  def findEquives(rewriteState: RewriteSearchState,
                  rules: Seq[Operator[RewriteSearchState]]): Set[Set[HyperTermId]] = {
    // Copy of graph is needed because we do not want merges to change our anchored nodes here.
    val res = new OperatorRunWithCaseSplit(equivDepth, splitDepth = Some(splitDepth), chooser = Some(randomChooser), preRunDepth = preRunDepth)
      .fromRewriteState(createTupeledGraph(rewriteState), rules.toSet)
    getTupledConclusions(res)
  }

  def findAndMergeEquives(rewriteState: RewriteSearchState,
                          rules: Seq[Operator[RewriteSearchState]]): RewriteSearchState = {
    val toMerge = findEquives(rewriteState, rules)
    assert(toMerge.flatten.intersect(rewriteState.graph.nodes) == toMerge.flatten)
    ObservationalEquivalence.mergeConclusions(rewriteState, toMerge.toSeq)
  }

  private val ltwfId = Identifier("ltwf")
  private val ltwfTransivity = new LetAction(new TranscalParser()("ltwf(?x, ?y) ||| ltwf(?z, x) >> ltwf(z, y)")).rules
  private val ltwfByConstructor = {
    constructors.flatMap(c => {
      val cType = c.root.annotation.get.subtrees.last
      val holesAndIgnores = c.root.annotation.get.subtrees.dropRight(1).zipWithIndex.map({
        case (t, i) if t == cType => AnnotatedTree.identifierOnly(Identifier(s"?param$i"))
        case _ => AnnotatedTree.identifierOnly(Identifier("_"))
      })
      val rootTree = AnnotatedTree.identifierOnly(Identifier("?root"))
      val lhs = AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(
        rootTree,
        AnnotatedTree.withoutAnnotations(c.root.copy(annotation = None), holesAndIgnores)
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
    })
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

    val term1 = typedTerm1.map(_.copy(annotation = None))
    val term2 = typedTerm2.map(_.copy(annotation = None))
    val relevantTypes = constructors.map(_.root.annotation.get match {
      case AnnotatedTree(Language.mapTypeId, trees, _) => trees.last
    }).toSet.filter(t => typedTerm1.nodes.map(_.root.annotation).contains(Some(t))
      || typedTerm2.nodes.map(_.root.annotation).contains(Some(t)))

    if (relevantTypes.isEmpty) return Set.empty

    assert(relevantTypes.size == 1)
    val ourType = relevantTypes.head

    // Create new rewrite rule for induction hypothesis
    // Need to demand that the used hypothesis works only on structures smaller than new applied constructor
    // For that i created the ltwf relation. The constructor rules will add the needed intial relations.
    // So I will do directed let and add ltwf(?params from correct type, PH0) on the premise
    val inductionPh = placeholders(ourType).head.copy(annotation = None)
    if ((!term1.nodes.map(_.root).contains(inductionPh)) || (!term2.nodes.map(_.root).contains(inductionPh)))
      return Set.empty

    logger.info(s"Trying to prove ${Programs.termToString(term1)} == ${Programs.termToString(term2)}")

    val updatedTerm1 = term1.map(identMapper(_, inductionPh))
    val updatedTerm2 = term2.map(identMapper(_, inductionPh))

    val hypoths = createHypothesis(term1, term2, inductionPh)

    val conses = constructors.filter(c =>
      (c.root.annotation.get match {
        case AnnotatedTree(Language.mapTypeId, trees, _) => trees.last
      }) == ourType)
    if (conses.forall(c => {
      // Replace inductionPh by inductionVar
      // Create base graph where inductionPh ||| c(existentials: _*)
      val constructedVal = AnnotatedTree.withoutAnnotations(c.root.copy(annotation = None),
        c.root.annotation.get.subtrees.dropRight(1).zipWithIndex.map({ case (_, i) =>
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
      logger.info(s"Found inductive rule: ${Programs.termToString(updatedTerm1)} == ${Programs.termToString(updatedTerm2)}")
      new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(updatedTerm1, updatedTerm2)),
        allowExistential = false).rules
    } else {
      logger.info(s"Proof Failed")
      Set.empty
    }
  }
}