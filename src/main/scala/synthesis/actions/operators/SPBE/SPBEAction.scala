package synthesis.actions.operators.SPBE

import structures.immutable.VersionedHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{Action, LetAction}
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import synthesis.search.Operator
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.collection.{immutable, mutable}

class SPBEAction(constantTeminals: Set[AnnotatedTree], changingTerminals: Seq[Seq[AnnotatedTree]], symbols: Set[AnnotatedTree], termDepth: Int = 5, equivDepth: Int = 8) extends Action {
  private val rules = SyGuSRewriteRules(symbols).rewriteRules

  private val placeholders: Seq[AnnotatedTree] = {
    def createPlaceholder(annotatedTree: AnnotatedTree, i: Int): AnnotatedTree =
      annotatedTree.copy(root = annotatedTree.root.copy(literal = s"Placeholder($i)"))

    for ((t, i) <- changingTerminals.head.zipWithIndex) yield createPlaceholder(t, i)
  }

  private val placeholderTerminals = constantTeminals ++ placeholders

  private val baseGraph = {
    if (placeholderTerminals.size > 1)
      Programs.destruct(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, placeholderTerminals.toSeq))
    else
      Programs.destruct(placeholderTerminals.head)
  }

  private def getEquives(rewriteRules: Set[Operator[RewriteSearchState]], terms: Seq[AnnotatedTree]): Set[Set[AnnotatedTree]] = {
    val (allInOne, root) = Programs.destructWithRoot(new AnnotatedTree(Language.semicolonId, terms.toList, Seq.empty))
    val top = allInOne.edges.find(e => e.target == root && e.edgeType == HyperTermIdentifier(Language.semicolonId)).head
    val anchorToTerm = top.sources.zipWithIndex.map(tAndI => (HyperEdge(tAndI._1, HyperTermIdentifier(Identifier(s"${Programs.termToString(terms(tAndI._2))}")), List.empty, NonConstructableMetadata), terms(tAndI._2))).toMap
    val anchors = anchorToTerm.keys.toSet
    val searchGraph = allInOne.addEdges(anchors)
    var rewriteState = new RewriteSearchState(searchGraph)
    for (i <- 1 to 10; op <- rewriteRules) rewriteState = op(rewriteState)
    val termToTarget: Map[AnnotatedTree, HyperTermId] = anchorToTerm.map(t => (t._2, rewriteState.graph.findEdges(t._1.edgeType).head.target))
    termToTarget.groupBy(t => t._2).map(_._2.keys.toSet).toSet
  }

  private def getRoots(rewriteState: RewriteSearchState) = {
    rewriteState.graph.edges.filter(_.edgeType.identifier == Language.typeId).map(_.sources.head)
  }

  private def shiftEdges(startId: Int, edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    edges.map(e => e.copy(target = e.target.copy(e.target.id + startId), sources = e.sources.map(hid => hid.copy(id = hid.id + startId))))
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val longRules = (0 until equivDepth).flatMap(_ => state.rewriteRules)
    var rewriteState = new RewriteSearchState(baseGraph)
    for (i <- 1 to termDepth) {
      // ******** SPBE ********
      // Gives a graph of depth i+~ applications of funcs on known terminals and functions
      rewriteState = rules.foldLeft(rewriteState)((s: RewriteSearchState, r: Operator[RewriteSearchState]) => r(s))
      val roots = getRoots(rewriteState)
      val rootToAnchors = roots.map(r => (r, changingTerminals.indices.map(index =>
            HyperEdge(r, HyperTermIdentifier(Identifier(s"anchor for ${r.id} iter: $index")), Seq.empty, NonConstructableMetadata)
          ))).toMap

      rewriteState =
        baseStep(rewriteState, longRules, rootToAnchors)
    }

    // Prove equivalence by induction.
    val roots = getRoots(rewriteState)
    val programs = Programs(rewriteState.graph)
    val newRules = for (r <- roots) yield {
      val terms = programs.reconstruct(r).take(100)
      // Do the induction step for each couple of terms
      terms.toSeq.combinations(2).map(it => (it(0), it(1))).map(inductionStep.tupled).collect({case Some(rr) => rr})
    }
    ActionSearchState(state.programs, state.rewriteRules ++ newRules.flatten)
    state
  }

  private def baseStep(rewriteState: RewriteSearchState,
                                               longRules: Seq[Operator[RewriteSearchState]],
                                               rootToAnchors: Map[HyperTermId, Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]]): RewriteSearchState = {
    // Use observational equivalence
    var state = rewriteState
    val fullGraph: VersionedHyperGraph[HyperTermId, HyperTermIdentifier] = fillPlaceholders(state, rootToAnchors)

    // ******** Observational equivalence ********
    // Use the anchors to create tuples for merging
    def idCreator = Stream.from(fullGraph.nodes.map(_.id).max).map(HyperTermId)

    // Using tuples to compare all equivalent terms over the different inputs  simultaneously
    val tupleEdges = (for ((r, vals) <- rootToAnchors) yield {
      val targets = vals.map(e => fullGraph.findEdges(e.edgeType).head.target)
      val newTarget = idCreator.head
      Seq(HyperEdge(newTarget, HyperTermIdentifier(Language.tupleId), targets, EmptyMetadata),
        HyperEdge(newTarget, HyperTermIdentifier(Identifier(s"anchor tuple for $r")), Seq.empty, NonConstructableMetadata))
    }).flatten

    val compressed = longRules.foldLeft(new RewriteSearchState(fullGraph.addEdges(tupleEdges.toSet)))(
      (s: RewriteSearchState, r: Operator[RewriteSearchState]) => r(s)
    )

    val toMerge = new mutable.HashMap[HyperTermId, mutable.Set[HyperTermId]]() with mutable.MultiMap[HyperTermId, HyperTermId]
    for (e <- compressed.graph.edges if e.edgeType.identifier.literal.startsWith("anchor tuple for ")) {
      toMerge.addBinding(e.target, HyperTermId(e.edgeType.identifier.literal.substring("anchor tuple for ".length).toInt))
    }

    for ((key, targets) <- toMerge;
         target <- targets) {
      state = new RewriteSearchState(state.graph.mergeNodes(key, target))
    }
    state
  }

  private def fillPlaceholders(rewriteState: RewriteSearchState,
                               rootToAnchors: Map[HyperTermId, Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]]): VersionedHyperGraph[HyperTermId, HyperTermIdentifier] = {
    // We will duplicate the graph for each symbolic input and change the placeholder to correct representation.
    val maxId = rewriteState.graph.nodes.map(_.id).max
    val fullGraph = VersionedHyperGraph((for ((newTerminals, index) <- changingTerminals.zipWithIndex) yield {
      // We know the placeholder doesnt have subtrees as we created it.
      // We want to create new anchors to find the relevant hypertermid later when searching equives in tuples
      val newEdges = for ((terminal, placeholder) <- newTerminals.zip(placeholders);
                          target = rewriteState.graph.findEdges(HyperTermIdentifier(placeholder.root)).head.target) yield {
        val newAnchors = rootToAnchors.values.map(_ (index)).toSet
        val (tempGraph, root) = Programs.destructWithRoot(terminal)
        val termGraph = tempGraph.mergeNodes(target, root)
        shiftEdges(maxId*index, (rewriteState.graph.addEdges(newAnchors).filter(_.edgeType.identifier != placeholder.root) ++ termGraph).toSet)
      }
      newEdges.flatten
    }).flatten: _*)
    fullGraph
  }

  private def inductionStep(state: ActionSearchState, term1: AnnotatedTree, term2: AnnotatedTree): Option[RewriteRule] = {
    // Create new rewrite rule for induction hypothesis
    val hypoth = new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(term1, term2))).rules

  }
}
