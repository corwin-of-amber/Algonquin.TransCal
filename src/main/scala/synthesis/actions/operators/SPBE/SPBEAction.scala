package synthesis.actions.operators.SPBE

import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{Action, LetAction}
import synthesis.rewrites.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.collection.mutable

// Constructors than split by datatype
// Grammar use this during graph expansion
class SPBEAction(typeBuilders: Set[AnnotatedTree], grammar: Set[AnnotatedTree], examples: Map[AnnotatedTree, Seq[AnnotatedTree]], termDepth: Int = 5, equivDepth: Int = 4) extends Action {
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
    case t => Set(t)
  })

  // How to do this? for now given by user
  // Maybe i should use the rewrite system
  // * Create sygus rules only from constructors.
  // * Create placeholders ahead of time
  // * Base graph of placeholders and constants of created type
  // * Find all hyper terms by type
  // * reconstruct
  // Or manually apply constructors - more control on created values, a bad assumption
  // * Assume you have a base value
  // * create placeholders as needed by constructors
  // * run each constructor on previous values by order or not
//  private val symbolicSamples = {
//    val allTypes = types.map(AnnotatedTree.identifierOnly) ++ constructors.flatMap(_.root.annotation.flatMap(_.subtrees))
//    val placehodlers = allTypes.flatMap(t => 0 until 3 map (i => createPlaceholder(t, i)))
////    for (t <- types;
////         vals = constants.filter(_.root.annotation.get.root == t);
////         consts = constructors.filter(_.root.annotation.get.subtrees.last.root == t)) yield {
////      val knownVals = mutable.Set[AnnotatedTree](vals.toSeq: _*)
////      for (i <- 0 until exampleDepth) yield {
////        val newVals = consts.map(c => AnnotatedTree.withoutAnnotations(c.root, ))
////      }
////    }
//  }

  private def createPlaceholder(placeholderType: AnnotatedTree, i: Int): Identifier =
    Identifier(literal = s"Placeholder($i)", annotation = Some(placeholderType))

  private val placeholders: Map[AnnotatedTree, Seq[Identifier]] =
    types.map(t => (t, 0 until 3 map (i => createPlaceholder(t, i)))).toMap

  val baseGraph: ActionSearchState.HyperGraph = {
    val symbols = placeholders.values.flatMap(ps => ps.map(AnnotatedTree.identifierOnly)) ++ constants
    if (symbols.size > 1)
      Programs.destruct(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, symbols.toSeq))
    else
      Programs.destruct(symbols.head)
  }

  private val idAnchorStart = "anchor for "
  private val tupleAnchorStart = "anchor tuple for "

  private def anchorByIndex(anchor: HyperEdge[HyperTermId, HyperTermIdentifier], index: Int) =
    anchor.copy(edgeType = HyperTermIdentifier(anchor.edgeType.identifier.copy(literal = anchor.edgeType.identifier.literal + s" iter: $index")))

  private def createAnchor(id: HyperTermId) =
    HyperEdge(id, HyperTermIdentifier(Identifier(s"$idAnchorStart$id")), Seq.empty, NonConstructableMetadata)

  private def getRoots(rewriteState: RewriteSearchState) =
    rewriteState.graph.findEdges(HyperTermIdentifier(Language.typeId)).map(_.sources.head)

  private def shiftEdges(startId: Int, edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =
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
      val terms = programs.reconstruct(r).take(20)
      // Do the induction step for each couple of terms
      (for ((term1, term2) <- terms.toSeq.combinations(2).map(it => (it(0), it(1)))) yield {
        inductionStep(state, term1, term2).collect({case rr => rr })
      }).flatten
    }
    ActionSearchState(state.programs, state.rewriteRules ++ newRules.flatten)
    state
  }

  def sygusStep(rewriteSearchState: RewriteSearchState): RewriteSearchState = {
    val res = rules.foldLeft(rewriteSearchState)((s: RewriteSearchState, r: Operator[RewriteSearchState]) => r(s))
    FunctionArgumentsAndReturnTypeRewrite(res)
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
    var fullGraph = structures.mutable.VersionedHyperGraph(noPlaceholdersGraph.toSeq: _*)
    for (((exampleKey, exampleValues), index) <- examples.zipWithIndex) {
      // We know the placeholder doesnt have subtrees as we created it.
      // We want to create new anchors to find the relevant hypertermid later when searching equives in tuples
      val newEdges = for ((terminal, placeholder) <- exampleValues.zip(placeholders(exampleKey));
                          target = graph.findEdges(HyperTermIdentifier(placeholder.copy(annotation = None))).head.target) yield {
        val newAnchors = roots.map(root => anchorByIndex(createAnchor(root), index))

        val termGraph = {
          val (tempGraph, root) = Programs.destructWithRoot(terminal, maxId = HyperTermId(noPlaceholdersGraph.nodes.map(_.id).max))
          tempGraph.mergeNodes(target, root)
        }

        val afterAddingTerm = noPlaceholdersGraph.edges ++ newAnchors ++ termGraph.edges
        val maxId = fullGraph.nodes.map(_.id).max
        shiftEdges(maxId, afterAddingTerm)
      }
      fullGraph ++= newEdges.flatten.toSet
    }
    fullGraph
  }

  def findEquives(rewriteState: RewriteSearchState,
                  rules: Seq[Operator[RewriteSearchState]]): mutable.MultiMap[HyperTermId, HyperTermId] = {
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
      val targets = 0 until examples.size map(i => fullGraph.findEdges(anchorByIndex(createAnchor(r), i).edgeType).head.target)
      val newTarget = idCreator()
      Seq(HyperEdge(newTarget, HyperTermIdentifier(Language.tupleId), targets.toList, EmptyMetadata),
        HyperEdge(newTarget, HyperTermIdentifier(Identifier(s"$tupleAnchorStart${r.id}")), Seq.empty, NonConstructableMetadata))
    }).flatten

    fullGraph.++=(tupleEdges)
    var searchState = new RewriteSearchState(fullGraph)
    for (_ <- 0 until equivDepth) {
      searchState = rules.foldLeft(searchState)(
        (s: RewriteSearchState, r: Operator[RewriteSearchState]) => r(s))
      logger.debug(s"Finished equiv rules round. Graph size is: ${searchState.graph.size}")
    }

    val toMerge = mutable.HashMultiMap.empty[HyperTermId, HyperTermId]
    for (e <- searchState.graph.edges if e.edgeType.identifier.literal.startsWith(tupleAnchorStart)) {
      toMerge.addBinding(e.target, HyperTermId(e.edgeType.identifier.literal.substring(tupleAnchorStart.length).toInt))
    }
    toMerge
  }

  private def findAndMergeEquives(rewriteState: RewriteSearchState,
                                  rules: Seq[Operator[RewriteSearchState]]): RewriteSearchState = {
    val toMerge = findEquives(rewriteState, rules)

    for ((key, targets) <- toMerge;
         target <- targets) {
      rewriteState.graph.mergeNodes(key, target)
    }
    rewriteState
  }

  private def inductionStep(state: ActionSearchState, term1: AnnotatedTree, term2: AnnotatedTree): Option[RewriteRule] = {
    // Each placeholder represents a value of a type.
    // To deal with multi param expressions some of the placeholders were duplicated ahead of time, so now just use 'em

    // Create new rewrite rule for induction hypothesis

    // TODO: Add constructor as part of action and use it to create steps
    val hypoth = new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(term1, term2))).rules
//    val mutualPlaceholders = term1.nodes.filter(_.root.literal.startsWith("Placeholder"))
//      .intersect(term2.nodes.filter(_.root.literal.startsWith("Placeholder")))
//    mutualPlaceholders.forall(p => {
//      // TODO: I might want to put all the rewrites on the same graph
//      val graph = Programs.destruct(term1.map(t => t))
//      true
////    })
//
//    // Change placeholder to differently named placeholders
//    val placeholderReplacers1: mutable.Map[Identifier, Seq[Identifier]] =
//      mutable.HashMap(placeholders.map(p =>
//        p.root -> (0 until term1.nodes.count(n => n == p)).map(i => p.root.copy(literal = s"${p.root.literal}$i"))
//      ):_ *)
//
//    val placeholderReplacers2: mutable.Map[Identifier, Seq[Identifier]] =
//      mutable.HashMap(placeholders.map(p =>
//        p.root -> (0 until term2.nodes.count(n => n == p)).map(i => p.root.copy(literal = s"${p.root.literal}$i"))
//      ):_ *)

//    val updatedTerm1 = {
//      placeholderReplacers1.map((k, phs) => )
//    }

    val allVars2 = placeholders.map(p => term2.nodes.count(_ == p))

//    val updatedTerm1 = term1.map(i => {
//      if(placeholders.map(_.root).contains(i)) {
//        placeholderCounters(i) = placeholderCounters(i) + 1
//        i.copy(literal = i.literal + placeholderCounters(i))
//      } else i
//    })



    // Need to rewrite the modified placeholder original expressions until equality

    None
  }
}
