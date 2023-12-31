package synthesis.actions.operators.SPBE

import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{Action, ElaborateAction, LetAction, OperatorRunAction}
import synthesis.rewrites.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

import scala.collection.mutable

// Constructors than split by datatype
// Grammar use this during graph expansion
class SPBEAction(typeBuilders: Set[AnnotatedTree], grammar: Set[AnnotatedTree], examples: Map[AnnotatedTree, Seq[AnnotatedTree]], termDepth: Int = 3, equivDepth: Int = 4) extends Action {
  assert(examples.values.map(_.size).toSet.size == 1)

  private def isFunctionType(annotatedTree: AnnotatedTree) = annotatedTree.root.annotation match {
    case Some(annotation) => annotation.root == Language.mapTypeId
    case None => false
  }

  private val constants = typeBuilders.filterNot(isFunctionType) ++ grammar.filterNot(isFunctionType)
  private val constructors = typeBuilders.filter(isFunctionType)
  private val rules = SyGuSRewriteRules(
    constructors ++
      grammar.filter(isFunctionType).map(t => t.copy(subtrees = Seq.empty))
  ).rewriteRules

  private val types: Set[AnnotatedTree] = constructors.flatMap(_.root.annotation.get match {
    case AnnotatedTree(Language.mapTypeId, trees, _) => trees
    case _ => throw new RuntimeException("All constructors should be function types")
  })

  // How to do this? for now given by user
  // Maybe i should use the rewrite system
  // * Create sygus rules only from constructors and grammer.
  // * Create placeholders ahead of time
  // * Base graph of placeholders and constants of created type
  // * Find all hyper terms by type
  // * reconstruct

  private def createPlaceholder(placeholderType: AnnotatedTree, i: Int): Identifier =
    Identifier(literal = s"Placeholder_${i}_type_{${Programs.termToString(placeholderType)}}", annotation = Some(placeholderType))

  private val placeholders: Map[AnnotatedTree, Seq[Identifier]] =
    types.map(t => (t, 0 to 1 map (i => createPlaceholder(t, i)))).toMap

  val baseGraph: ActionSearchState.HyperGraph = {
    val symbols = placeholders.values.flatMap(ps => ps.map(AnnotatedTree.identifierOnly)) ++ constants
    if (symbols.size > 1)
      Programs.destruct(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, symbols.toSeq))
    else
      Programs.destruct(symbols.head)
  }

  val untypedBaseGraph: ActionSearchState.HyperGraph = {
    val symbols = placeholders.values.flatMap(ps => ps.map(AnnotatedTree.identifierOnly)) ++ constants
    if (symbols.size > 1)
      Programs.destruct(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId,
        symbols.map(_.map(_.copy(annotation = None))).toSeq))
    else
      Programs.destruct(symbols.head.map(_.copy(annotation = None)))
  }

  private val idAnchorStart = "anchor for "
  private val tupleAnchorStart = "anchor tuple for "

  private def anchorByIndex(anchor: HyperEdge[HyperTermId, HyperTermIdentifier], index: Int) =
    anchor.copy(edgeType = HyperTermIdentifier(anchor.edgeType.identifier.copy(literal = anchor.edgeType.identifier.literal + s" iter: $index")))

  private def createAnchor(id: HyperTermId) =
    HyperEdge(id, HyperTermIdentifier(Identifier(s"$idAnchorStart$id")), Seq.empty, NonConstructableMetadata)

  private def getRoots(rewriteState: RewriteSearchState) =
    rewriteState.graph.findEdges(HyperTermIdentifier(Language.typeId)).map(_.sources.head)

  private def shiftEdges(startId: Int, edges: collection.Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =
    edges.map(e => e.copy(target = e.target.copy(e.target.id + startId), sources = e.sources.map(hid => hid.copy(id = hid.id + startId))))

  override def apply(state: ActionSearchState): ActionSearchState = {
    var rewriteState = new RewriteSearchState(baseGraph)

    for (_ <- 1 to termDepth) {
      // ******** SPBE ********

      // Gives a graph of depth i+~ applications of funcs on known terminals and functions
      rewriteState = sygusStep(rewriteState)
      rewriteState = findAndMergeEquives(rewriteState, state.rewriteRules.toSeq)
    }

    // Prove equivalence by induction.
    val roots = getRoots(rewriteState)
    val programs = Programs(rewriteState.graph)
    val newRules = for (r <- roots) yield {
      val terms = programs.reconstruct(r).take(20).toList
      // Do the induction step for each couple of terms
      (for ((term1, term2) <- terms.combinations(2).toSeq.map(it => (it(0), it(1)))) yield {
        inductionStep(state, term1, term2).collect({ case rr => rr })
      }).flatten
    }
    ActionSearchState(state.programs, state.rewriteRules ++ newRules.flatten)
    state
  }

  def sygusStep(rewriteSearchState: RewriteSearchState): RewriteSearchState = {
    val res = rules.map((r: Operator[RewriteSearchState]) => r(rewriteSearchState.deepCopy()))
    val newState = res.map(_.graph).foldLeft(rewriteSearchState)((state, es) => {
      state.graph ++= shiftEdges(state.graph.map(_.target.id).max, es)
      state
    })
    FunctionArgumentsAndReturnTypeRewrite(newState)
  }


  /** Each placeholder will be switched to the appropriate terminals and anchors will be added appropriately.
    *
    * @param graph - basic graph with placeholders and anchors on roots
    * @return A graph duplicated so all placeholders were switched to the changing terms
    */
  private def switchPlaceholders(graph: RewriteSearchState.HyperGraph): RewriteSearchState.HyperGraph = {
    // We will duplicate the graph for each symbolic input and change the placeholder to correct representation.
    val updatedPlaceholders = placeholders.values.flatMap(_.map(_.copy(annotation = None))).toSet
    val noPlaceholdersGraph = graph.filterNot(e => updatedPlaceholders.contains(e.edgeType.identifier))
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

    // ******** Observational equivalence ********
    // Use the anchors to create tuples for merging
    val idCreator: () => HyperTermId = {
      var stream = Stream.from(fullGraph.nodes.map(_.id).max + 1).map(HyperTermId)
      () => {
        stream = stream.tail
        stream.head
      }
    }

    // Using tuples to compare all equivalent terms over the different inputs  simultaneously
    val tupleEdges = (for (r <- roots) yield {
      val anchorsByIndex = examples.head._2.indices map (i => anchorByIndex(createAnchor(r), i))
      val targets = anchorsByIndex.map(a => fullGraph.findEdges(a.edgeType).head.target)
      val newTarget = idCreator()
      Seq(HyperEdge(newTarget, HyperTermIdentifier(Language.tupleId), targets.toList, EmptyMetadata),
        HyperEdge(newTarget, HyperTermIdentifier(Identifier(s"$tupleAnchorStart${r.id}")), Seq.empty, NonConstructableMetadata))
    }).flatten

    fullGraph.++=(tupleEdges)
    val searchState = new OperatorRunAction(equivDepth)(ActionSearchState(Programs(fullGraph.filterNot(_.edgeType.identifier.literal == "type")), rules.toSet))

    val toMerge = mutable.HashMultiMap.empty[HyperTermId, HyperTermId]
    for (e <- searchState.programs.hyperGraph.edges if e.edgeType.identifier.literal.startsWith(tupleAnchorStart)) {
      toMerge.addBinding(e.target, HyperTermId(e.edgeType.identifier.literal.substring(tupleAnchorStart.length).toInt))
    }

    toMerge.values.collect({ case s: mutable.Set[HyperTermId] if s.size > 1 => s.toSet }).toSet
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

  def inductionStep(state: ActionSearchState, typedTerm1: AnnotatedTree, typedTerm2: AnnotatedTree): Set[RewriteRule] = {
    // Each placeholder represents a value of a type.
    // To deal with multi param expressions some of the placeholders were duplicated ahead of time, so now just use 'em

    val term1 = typedTerm1.map(_.copy(annotation = None))
    val term2 = typedTerm2.map(_.copy(annotation = None))
    val relevantTypes = constructors.map(_.root.annotation.get match {
      case AnnotatedTree(Language.mapTypeId, trees, _) => trees.last
    }).filter(t => typedTerm1.nodes.map(_.root.annotation).contains(Some(t))
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

    logger.debug(s"Trying to prove ${Programs.termToString(term1)} == ${Programs.termToString(term2)}")

    def identMapper(i: Identifier) = i match {
      case i if i == inductionPh => i.copy(literal = "?inductionVar")
      case i if i.literal.startsWith("Placeholder") => i.copy(literal = "?" + i.literal)
      case i => i
    }

    def cleanVars(i: Identifier): Identifier = if (i.literal.startsWith("?")) i.copy(literal = i.literal.drop(1)) else i

    val updatedTerm1 = term1.map(identMapper)
    val updatedTerm2 = term2.map(identMapper)

    val hypoths = {
      val cleanTerm1 = term1.map({
        case i if i == inductionPh => i.copy(literal = "?SomeVar")
        case i if i.literal.startsWith("Placeholder") => i.copy(literal = "?" + i.literal)
        case i => i
      })
      val cleanTerm2 = term2.map({
        case i if i == inductionPh => i.copy(literal = "?SomeVar")
        case i if i.literal.startsWith("Placeholder") => i.copy(literal = "?" + i.literal)
        case i => i
      })
      val precondition = AnnotatedTree.withoutAnnotations(ltwfId,
        Seq(Identifier("?SomeVar"), cleanVars(identMapper(inductionPh))).map(AnnotatedTree.identifierOnly))

      val dir1 = new LetAction(AnnotatedTree.withoutAnnotations(Language.directedLetId, Seq(
        AnnotatedTree.withoutAnnotations(Language.trueCondBuilderId, Seq(precondition, cleanTerm1)),
        cleanTerm2
      ))).rules
      val dir2 = new LetAction(AnnotatedTree.withoutAnnotations(Language.directedLetId, Seq(
        AnnotatedTree.withoutAnnotations(Language.trueCondBuilderId, Seq(precondition, cleanTerm2)),
        cleanTerm1
      ))).rules
      dir1 ++ dir2
    }

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
      val inductionPhTree = AnnotatedTree.identifierOnly(cleanVars(identMapper(inductionPh)))
      val anchorForTerm1 = AnnotatedTree.identifierOnly(Identifier("AnchorForTerm1"))
      val anchorForTerm2 = AnnotatedTree.identifierOnly(Identifier("AnchorForTerm2"))
      val (pattern, patternRoot) = Programs.destructPatternsWithRoots(Seq(AnnotatedTree.withoutAnnotations(Language.andCondBuilderId,
        Seq(anchorForTerm1, anchorForTerm2)))).head
//      val indIsLTWFThenPh = AnnotatedTree(Language.andCondBuilderId,
//        AnnotatedTree.identifierOnly(Language.trueId)
//        AnnotatedTree(ltwfId, Seq(inductionPhTree, AnnotatedTree.identifierOnly(cleanVars(inductionPh))), Seq.empty)
//      , Seq.empty)
      val term1WithAnchor = AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(updatedTerm1.map(cleanVars), anchorForTerm1))
      val term2WithAnchor = AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(updatedTerm2.map(cleanVars), anchorForTerm2))
//      val phToConstructed = new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(inductionPhTree, constructedVal)))      val actionSearchState = new ActionSearchState(Programs(term1WithAnchor).addTerm(term2WithAnchor).addTerm(indIsLTWFThenPh), rules ++ ltfwRules ++ hypoths ++ phToConstructed.rules ++ state.rewriteRules)
      val phToConstructed = new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(inductionPhTree, constructedVal)))
      val actionSearchState = new ActionSearchState(Programs(term1WithAnchor).addTerm(term2WithAnchor), rules ++ ltfwRules ++ hypoths ++ phToConstructed.rules ++ state.rewriteRules)
      val elaborateState = new ElaborateAction(HyperTermIdentifier(anchorForTerm1.root), pattern, patternRoot, maxSearchDepth = Some(8))(actionSearchState)
      // If they are different it was found
      elaborateState != actionSearchState
    })) {
      logger.info(s"Found inductive rule: ${Programs.termToString(updatedTerm1)} == ${Programs.termToString(updatedTerm2)}")
      new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(updatedTerm1, updatedTerm2))).rules
    } else Set.empty
  }
}
