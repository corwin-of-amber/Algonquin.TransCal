package synthesis.actions.operators.SPBE

import structures.immutable.VersionedHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{Action, LetAction}
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.Operator
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.collection.mutable

class SPBEAction(constantTeminals: Set[AnnotatedTree], changingTerminals: Seq[Seq[AnnotatedTree]], symbols: Set[AnnotatedTree], termDepth: Int = 5, equivDepth: Int = 8) extends Action {
  private val rules = SyGuSRewriteRules(symbols).rewriteRules

  private val placeholders: Seq[AnnotatedTree] = {
    def createPlaceholder(annotatedTree: AnnotatedTree, i: Int): AnnotatedTree =
      annotatedTree.copy(root = annotatedTree.root.copy(literal = s"Placeholder($i)"))

    for ((t, i) <- changingTerminals.head.zipWithIndex) yield createPlaceholder(t, i)
  }

  private val placeholderTerminals = constantTeminals ++ placeholders

  val baseGraph: HyperGraph = {
    if (placeholderTerminals.size > 1)
      Programs.destruct(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, placeholderTerminals.toSeq))
    else
      Programs.destruct(placeholderTerminals.head)
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
    val longRules = (0 until equivDepth).flatMap(_ => state.rewriteRules)
    var rewriteState = new RewriteSearchState(baseGraph)

    for (i <- 1 to termDepth) {
      // ******** SPBE ********

      // Gives a graph of depth i+~ applications of funcs on known terminals and functions
      rewriteState = sygusStep(rewriteState)
      rewriteState = findAndMergeEquives(rewriteState, longRules)
    }

    // Prove equivalence by induction.
    val roots = getRoots(rewriteState)
    val programs = Programs(rewriteState.graph)
    val newRules = for (r <- roots) yield {
      val terms = programs.reconstruct(r).take(100)
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
  private def switchPlaceholders(graph: RewriteSearchState.HyperGraph): VersionedHyperGraph[HyperTermId, HyperTermIdentifier] = {
    // We will duplicate the graph for each symbolic input and change the placeholder to correct representation.
    val noPlaceholdersGraph = graph.filterNot(e => placeholders.map(_.root.copy(annotation = None)).contains(e.edgeType.identifier))
    val roots = getRoots(new RewriteSearchState(noPlaceholdersGraph))
    var fullGraph = noPlaceholdersGraph
    for ((newTerminals, index) <- changingTerminals.zipWithIndex) {
      // We know the placeholder doesnt have subtrees as we created it.
      // We want to create new anchors to find the relevant hypertermid later when searching equives in tuples
      val newEdges = for ((terminal, placeholder) <- newTerminals.zip(placeholders);
                          target = graph.findEdges(HyperTermIdentifier(placeholder.root.copy(annotation = None))).head.target) yield {
        val newAnchors =
          roots.map(root => anchorByIndex(createAnchor(root), index))

        val termGraph = {
          val (tempGraph, root) = Programs.destructWithRoot(terminal, maxId = HyperTermId(noPlaceholdersGraph.nodes.map(_.id).max))
          tempGraph.mergeNodes(target, root)
        }

        val afterAddingTerm = noPlaceholdersGraph.++(newAnchors).++(termGraph.edges).edges
        val maxId = fullGraph.nodes.map(_.id).max
        shiftEdges(maxId, afterAddingTerm)
      }
      fullGraph = fullGraph.++(newEdges.flatten.toSet)
    }
    fullGraph
  }

  def findEquives(rewriteState: RewriteSearchState,
                          longRules: Seq[Operator[RewriteSearchState]]): mutable.MultiMap[HyperTermId, HyperTermId] = {
    // Use observational equivalence
    val roots = getRoots(rewriteState)
    val fullGraph: VersionedHyperGraph[HyperTermId, HyperTermIdentifier] =
      switchPlaceholders(rewriteState.graph)

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
      val targets = changingTerminals.indices.map(i => fullGraph.findEdges(anchorByIndex(createAnchor(r), i).edgeType).head.target)
      val newTarget = idCreator()
      Seq(HyperEdge(newTarget, HyperTermIdentifier(Language.tupleId), targets.toList, EmptyMetadata),
        HyperEdge(newTarget, HyperTermIdentifier(Identifier(s"$tupleAnchorStart${r.id}")), Seq.empty, NonConstructableMetadata))
    }).flatten

    val compressed = longRules.foldLeft(new RewriteSearchState(fullGraph.++(tupleEdges)))(
      (s: RewriteSearchState, r: Operator[RewriteSearchState]) => r(s)
    )

    val toMerge = mutable.HashMultiMap.empty[HyperTermId, HyperTermId]
    for (e <- compressed.graph.edges if e.edgeType.identifier.literal.startsWith(tupleAnchorStart)) {
      toMerge.addBinding(e.target, HyperTermId(e.edgeType.identifier.literal.substring(tupleAnchorStart.length).toInt))
    }
    toMerge
  }

  private def findAndMergeEquives(rewriteState: RewriteSearchState,
                                               longRules: Seq[Operator[RewriteSearchState]]): RewriteSearchState = {
    var state = rewriteState
    val toMerge = findEquives(rewriteState, longRules)

    for ((key, targets) <- toMerge;
         target <- targets) {
      state = new RewriteSearchState(state.graph.mergeNodes(key, target))
    }
    state
  }

  private def inductionStep(state: ActionSearchState, term1: AnnotatedTree, term2: AnnotatedTree): Option[RewriteRule] = {
    def replaceByOrder(annotatedTree: AnnotatedTree, original: Identifier, identifierSeq: Seq[Identifier]) = {
      val it = identifierSeq.iterator
      annotatedTree.map(i => if (i == original) it.next() else i)
    }

    // Change placeholder to differently named placeholders
    val placeholderReplacers1: mutable.Map[Identifier, Seq[Identifier]] =
      mutable.HashMap(placeholders.map(p =>
        p.root -> (0 until term1.nodes.count(n => n == p)).map(i => p.root.copy(literal = s"${p.root.literal}$i"))
      ):_ *)

    val placeholderReplacers2: mutable.Map[Identifier, Seq[Identifier]] =
      mutable.HashMap(placeholders.map(p =>
        p.root -> (0 until term2.nodes.count(n => n == p)).map(i => p.root.copy(literal = s"${p.root.literal}$i"))
      ):_ *)

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

    // Create new rewrite rule for induction hypothesis
    val hypoth = new LetAction(AnnotatedTree.withoutAnnotations(Language.letId, Seq(term1, term2))).rules
    // Need to rewrite the modified placeholder original expressions until equality

    None
  }
}
