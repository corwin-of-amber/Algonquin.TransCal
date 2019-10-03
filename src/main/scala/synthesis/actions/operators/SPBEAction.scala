package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.actions.ActionSearchState.HyperGraph
import synthesis.rewrites.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

import scala.collection.mutable

// Constructors than split by datatype
// Grammar use this during graph expansion
class SPBEAction(typeBuilders: Set[AnnotatedTree], grammar: Set[AnnotatedTree], examples: Map[AnnotatedTree, Seq[AnnotatedTree]], termDepth: Int = 3, equivDepth: Int = 4) extends Action {
  assert(examples.values.map(_.size).toSet.size == 1)

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
    Identifier(literal = s"Placeholder_${i}_type_{${Programs.termToString(placeholderType)}}", annotation = Some(placeholderType))

  private def anchorByIndex(anchor: HyperEdge[HyperTermId, HyperTermIdentifier], index: Int) =
    anchor.copy(edgeType = HyperTermIdentifier(anchor.edgeType.identifier.copy(literal = anchor.edgeType.identifier.literal + s" iter: $index")))

  private def createAnchor(id: HyperTermId) =
    HyperEdge(id, HyperTermIdentifier(Identifier(s"$idAnchorStart$id")), Seq.empty, NonConstructableMetadata)

  private def getRoots(rewriteState: RewriteSearchState) =
    rewriteState.graph.findEdges(HyperTermIdentifier(Language.typeId)).map(_.sources.head)

  private def shiftEdges(startId: Int, edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =
    edges.map(e => e.copy(target = e.target.copy(e.target.id + startId), sources = e.sources.map(hid => hid.copy(id = hid.id + startId))))

  private val constants = typeBuilders.filterNot(isFunctionType) ++ grammar.filterNot(isFunctionType)

  private val constructors = typeBuilders.filter(isFunctionType)

  private val rules = SyGuSRewriteRules(
    constructors ++
      grammar.filter(isFunctionType).map(t => t.copy(subtrees = Seq.empty))
  ).rewriteRules

  private val types: Set[AnnotatedTree] = (constructors ++ grammar.filter(isFunctionType).map(t => t.copy(subtrees = Seq.empty)))
    .flatMap(_.root.annotation.get match {
      case AnnotatedTree(Language.mapTypeId, trees, _) => trees
      case _ => throw new RuntimeException("All constructors should be function types")
    })

  private val placeholders: Map[AnnotatedTree, Seq[Identifier]] =
    types.map(t => (t, 0 to 1 map (i => createPlaceholder(t, i)))).toMap

  private def createBaseGraph(typed: Boolean): HyperGraph = {
    val symbols = placeholders.values.flatMap(ps => ps.map(AnnotatedTree.identifierOnly)).toSeq ++ constants
    val symbolsToUse = if(typed) symbols else symbols.map(_.map(_.copy(annotation = None)))
    if (symbols.size > 1)
      Programs.destruct(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, symbolsToUse))
    else
      Programs.destruct(symbolsToUse.head)
  }

  val baseGraph: ActionSearchState.HyperGraph = createBaseGraph(true)

  val untypedBaseGraph: ActionSearchState.HyperGraph = createBaseGraph(false)

  private val idAnchorStart = "anchor for "
  private val tupleAnchorStart = "anchor tuple for "

  override def apply(initialState: ActionSearchState): ActionSearchState = {
    var rewriteState = new RewriteSearchState(baseGraph)
    var state = initialState
    val foundRules = mutable.Buffer.empty[mutable.Buffer[(AnnotatedTree, AnnotatedTree)]]

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
      val roots = getRoots(rewriteState)
      val programs = Programs(rewriteState.graph)
      for (r <- roots) {
        val terms = programs.reconstruct(r).toList
        // Before running induction steps, should filter out some of the terms.
        // To do that I will run an observational equivalence step and insert temporary rules.
        // Because we might need these temporary rules to help with future induction rules.
        logger.info("Filtering terms that don't need induction using observational equivalence")
        val equives = new ObservationalEquivalence(equivDepth).fromTerms(terms, state.rewriteRules)
        // Take smallest of each set as heuristic to have no unneeded subterms
        val filteredTerms = equives.map(_.minBy(_.size)).toSeq
        // Do the induction step for each couple of terms
        for ((term1, term2) <- filteredTerms.combinations(2).toSeq.map(it => (it(0), it(1)))) {
          val res = inductionStep(state, term1, term2).collect({ case rr => rr })
          if (res.nonEmpty) {
            state = ActionSearchState(state.programs, state.rewriteRules ++ res)
            foundRules.last.+=((term1, term2))
          }
        }
      }
      logger.info(s"Found new lemmas in depth $i:")
      for((t1, t2) <- foundRules.last)
        logger.info(s"  ${Programs.termToString(t1)} == ${Programs.termToString(t2)}")
    }
    state
  }

  def sygusStep(rewriteSearchState: RewriteSearchState): RewriteSearchState = {
    val res = rules.foldLeft(rewriteSearchState)((state, r) => r(state))
    FunctionArgumentsAndReturnTypeRewrite(res)
  }


  /** Each placeholder will be switched to the appropriate terminals and anchors will be added appropriately.
    *
    * @param graph - basic graph with placeholders and anchors on roots
    * @return A graph duplicated so all placeholders were switched to the changing terms
    */
  private def switchPlaceholders(graph: RewriteSearchState.HyperGraph): RewriteSearchState.HyperGraph = {
    // We will duplicate the graph for each symbolic input and change the placeholder to correct representation.
    val untypedPlaceholders = placeholders.values.flatMap(_.map(_.copy(annotation = None))).toSet
    val noPlaceholdersGraph = graph.filterNot(e => untypedPlaceholders.contains(e.edgeType.identifier))
    val roots = getRoots(new RewriteSearchState(noPlaceholdersGraph))
    val fullGraph = structures.mutable.CompactHyperGraph(noPlaceholdersGraph.toSeq: _*)
    for ((exampleKey, exampleValues) <- examples) {
      // We know the placeholder doesnt have subtrees as we created it.
      // We want to create new anchors to find the relevant hypertermid later when searching equives in tuples
      for ((terminal, index) <- exampleValues.zipWithIndex;
           placeholder = placeholders(exampleKey).head;
           target = graph.findEdges(HyperTermIdentifier(placeholder.copy(annotation = None))).head.target) {
        val newAnchors = roots.map(root => anchorByIndex(createAnchor(root), index))

        val termGraph = {
          val (tempGraph, root) = Programs.destructWithRoot(terminal, maxId = HyperTermId(noPlaceholdersGraph.nodes.map(_.id).max))
          tempGraph.mergeNodes(target, root)
        }

        val afterAddingTerm = noPlaceholdersGraph.edges ++ newAnchors ++ termGraph.edges
        val maxId = fullGraph.nodes.map(_.id).max
        val afterShift = shiftEdges(maxId, afterAddingTerm).filterNot(e => {
          val literal = e.edgeType.identifier.literal
          literal.startsWith(idAnchorStart) && !literal.contains("iter:")
        })
        fullGraph ++= afterShift
      }
    }
    fullGraph
  }

  def findEquives(rewriteState: RewriteSearchState,
                  rules: Seq[Operator[RewriteSearchState]]): Set[Set[HyperTermId]] = {
    // Use observational equivalence
    val roots = getRoots(rewriteState)
    val fullGraph: RewriteSearchState.HyperGraph =
      switchPlaceholders(rewriteState.graph.++(roots.map(r => createAnchor(r))))
        .filterNot(_.edgeType.identifier == Language.typeId)

    // ******** Observational equivalence ********
    // Use the anchors to create tuples for merging
    val idCreator: () => HyperTermId = {
      var last = fullGraph.nodes.map(_.id).max
      () => {last += 1; HyperTermId(last)}
    }

    // Using tuples to compare all equivalent terms over the different inputs  simultaneously
    val tupleEdges = (for (r <- roots) yield {
      val anchorsByIndex = examples.head._2.indices map (i => anchorByIndex(createAnchor(r), i))
      val targets = anchorsByIndex.map(a => fullGraph.findEdges(a.edgeType).head.target)
      val newTarget = idCreator()
      val anchor = ObservationalEquivalence.createAnchor(r)
      Seq(HyperEdge(newTarget, HyperTermIdentifier(Language.tupleId), targets.toList, EmptyMetadata),
        anchor.copy(target = newTarget))
    }).flatten

    fullGraph.++=(tupleEdges)
    logger.info("Running Observational Equivalence on symbolic examples")
    new ObservationalEquivalenceWithCaseSplit(equivDepth)
      .getEquivesFromRewriteState(RewriteSearchState(fullGraph), rules.toSet)._2
      .filter(_.size > 1)
  }

  private def findAndMergeEquives(rewriteState: RewriteSearchState,
                                  rules: Seq[Operator[RewriteSearchState]]): RewriteSearchState = {
    val toMerge = findEquives(rewriteState, rules)

    for (targets <- toMerge;
         source = targets.head;
         target <- targets.tail) {
      rewriteState.graph.mergeNodesInPlace(source, target)
    }
    rewriteState
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

  private def cleanVars(i: Identifier): Identifier = if (i.literal.startsWith("?")) i.copy(literal = i.literal.drop(1)) else i

  private def identMapper(i: Identifier, inductionPh: Identifier, inductionVarName: String = "?inductionVar") = i match {
    case i if i == inductionPh => i.copy(literal = inductionVarName)
    case i if i.literal.startsWith("Placeholder") => i.copy(literal = "?" + i.literal)
    case i => i
  }

  private def createHypothesis(term1: AnnotatedTree, term2: AnnotatedTree, inductionPh: Identifier, replacementVarLiteral: String = "?SomeVar") = {
    val hypothIndcVar = "?SomeVar"
    val (cleanTerm1, cleanTerm2) = (term1.map(identMapper(_, inductionPh, hypothIndcVar)), term2.map(identMapper(_, inductionPh, hypothIndcVar)))

    val precondition = AnnotatedTree.withoutAnnotations(ltwfId,
      Seq(Identifier("?SomeVar"), cleanVars(identMapper(inductionPh, inductionPh))).map(AnnotatedTree.identifierOnly))

    // Precondition on each direction of the hypothesis
    Seq((cleanTerm1, cleanTerm2), (cleanTerm2, cleanTerm1)).flatMap({case (t1, t2) =>
      new LetAction(AnnotatedTree.withoutAnnotations(Language.directedLetId, Seq(
        AnnotatedTree.withoutAnnotations(Language.trueCondBuilderId, Seq(precondition, t1)), t2
      ))).rules})
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
        c.root.annotation.get.subtrees.dropRight(1).zipWithIndex.map({case (_, i) =>
          AnnotatedTree.identifierOnly(Identifier(s"param$i"))
        })
      )
      val phToConstructed = new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(
        AnnotatedTree.identifierOnly(cleanVars(identMapper(inductionPh, inductionPh))),
        constructedVal
      )))

      val cleanUpdatedTerms = Seq(updatedTerm1, updatedTerm2).map(_.map(cleanVars))
      val actionState = ActionSearchState(Programs(cleanUpdatedTerms.head).addTerm(cleanUpdatedTerms.last), ltfwRules ++ hypoths ++ phToConstructed.rules ++ state.rewriteRules)
      val nextState = new ObservationalEquivalenceWithCaseSplit(equivDepth)(actionState)
      val pattern = Programs.destructPattern(AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, cleanUpdatedTerms))
      nextState.programs.hyperGraph.findSubgraph[Int](pattern).nonEmpty
    })) {
      logger.info(s"Found inductive rule: ${Programs.termToString(updatedTerm1)} == ${Programs.termToString(updatedTerm2)}")
      new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(updatedTerm1, updatedTerm2)), allowExistential = false).rules
    } else {
      logger.info(s"Proof Failed")
      Set.empty
    }
  }
}
