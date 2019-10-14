package synthesis.actions.operators

import structures.{HyperEdge, IdMetadata, Uid}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.ObservationalEquivalence.{anchorStart, createAnchor}
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{Identifier, Language}


/** Case split action splits a variable into options specified by the splitter edge. Variable edges are deleted to
  * prevent merging of possible values.
  * A new graph is created containing tuples of each expression in the original graph where each tuple is the
  * expression with the value of the variable changed to the appropriate value.
  * Than symbolic  observational equivalence will be run and the results are than matched and nodes that should merge
  * will be merged in the result state.
  *
  * Input graph should contain an edge of type possibleSplit where the target is SplitTrue and the first source is the
  * variable to change and possible values are the rest of the sources.
  */
class CaseSplitAction(splitterChooser: Option[CaseSplitAction.SplitChooser],
                      splitDepthOption: Option[Int],
                      maxDepthOption: Option[Int]) extends Action {
  private val splitDepth = splitDepthOption.getOrElse(1)
  private val maxDepth = maxDepthOption.getOrElse(4)

  def this(splitters: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]],
           maxDepthOption: Option[Int]) =
    this(Some(CaseSplitAction.specificChooser(splitters)), Some(splitters.length), maxDepthOption)

  def this(splitter: HyperEdge[HyperTermId, HyperTermIdentifier],
           maxDepthOption: Option[Int] = None) =
    this(Seq(splitter), maxDepthOption)

  val obvEquiv = new ObservationalEquivalence(maxDepth = maxDepth)

  def getFoundConclusionsFromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]])
  : Set[Set[HyperTermId]] = {
    val equives = innerGetFoundConclusionsFromRewriteState(state, rules, Seq.empty)
    val toMerge = equives.head.filter(_.size > 1).flatMap(hyperTermIds => {
      val restEquives = equives.tail
      for (i <- hyperTermIds; relevantSets = restEquives.map(_.find(_.contains(i)).get)) yield {
        relevantSets.fold(hyperTermIds)((distjuinted, set) => distjuinted.intersect(set))
      }
    }).filter(_.size > 1)
    toMerge
  }

  private def innerGetFoundConclusionsFromRewriteState(state: RewriteSearchState,
                                                       rules: Set[Operator[RewriteSearchState]],
                                                       chosen: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]])
  : Set[Set[Set[HyperTermId]]] = {
    val chooser = splitterChooser.getOrElse(randomChooser())
    val withAnchors =
      if (state.graph.edgeTypes.exists(_.identifier.literal.startsWith(anchorStart))) state.graph
      else state.graph ++ state.graph.map(e => createAnchor(e.target))
    val splitters = chooser(state, chosen).toSeq
    splitters.zipWithIndex.flatMap({ case (splitter, i) =>
      val tupledGraph = {
        // Need to shift all edges on graph because we want to prevent one changed value from affecting parts of other
        // values. Working on edges to prevent unwanted compaction until adding back anchors and adding new tuples.
        val initialEdges = withAnchors.edges.filter(e => (!e.edgeType.identifier.literal.startsWith(anchorStart))
          && e.target != splitter.sources.head && !splitters.take(i + 1).contains(e))
        val shiftedEdgesWithShiftSize = splitter.sources.tail.tail
          .foldLeft(Seq((CaseSplitAction.mergeNodes(initialEdges, splitter.sources(1), splitter.sources.head), 0)))(
            (all, id) => {
              val updatedEdges = CaseSplitAction.mergeNodes(initialEdges, id, splitter.sources.head)
              val shiftSize = all.last._2 + CaseSplitAction.edgesMax(all.last._1) + 1
              all :+ (CaseSplitAction.shiftEdges(shiftSize, updatedEdges), shiftSize)
            })
        val newIds = Stream.from(CaseSplitAction.edgesMax(shiftedEdgesWithShiftSize.last._1)).map(HyperTermId)
        val tuplesAndAnchors = withAnchors.filter(_.edgeType.identifier.literal.startsWith(anchorStart))
          .zip(newIds).flatMap({ case (e, newId) =>
          // Move anchor to a new id from the id stream and then create a tuple of values from each replacer
          Set(e.copy(target = newId),
            HyperEdge(newId,
              HyperTermIdentifier(Language.tupleId),
              shiftedEdgesWithShiftSize.zipWithIndex.map({ case ((_, shift), i) =>
                // TODO: fix it so splitted will point to sons
                if (e.target == splitter.sources.head) HyperTermId(splitter.sources.tail(i).id + shift)
                else HyperTermId(e.target.id + shift)
              }),
              NonConstructableMetadata)
          )
        })
        val res = new RewriteSearchState.HyperGraph(shiftedEdgesWithShiftSize.flatMap(_._1).toSet ++ tuplesAndAnchors)
        res
      }

      // Observational Equivalence will use anchors in graph and calculate hyper term id from their identifiers.
      if (chosen.length + 1 < splitDepth)
        innerGetFoundConclusionsFromRewriteState(RewriteSearchState(tupledGraph), rules, chosen :+ splitter)
      else {
        val res = obvEquiv.getEquivesFromRewriteState(RewriteSearchState(tupledGraph), rules)
        Set(res._2)
      }
    })
    }.toSet

  def getFoundConclusions(state: ActionSearchState): Set[Set[HyperTermId]] = {
    getFoundConclusionsFromRewriteState(new RewriteSearchState(state.programs.hyperGraph), state.rewriteRules)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val rState = new RewriteSearchState(state.programs.hyperGraph)
    val toMerge = getFoundConclusionsFromRewriteState(rState, state.rewriteRules).toSeq
    val newState = ObservationalEquivalence.mergeConclusions(rState, toMerge)
    ActionSearchState(Programs(newState.graph.filterNot(_.edgeType.identifier.literal.startsWith("caseAnchor"))),
      state.rewriteRules)
  }

  def randomChooser(depth: Int = maxDepth): CaseSplitAction.SplitChooser = {
    val depthUids = (0 to splitDepth).map(_ => IdMetadata(new Uid))
    (state: RewriteSearchState, chose: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) => {
      //      val uidsAbove = depthUids.take(chose.length)
      //      chose.zip(uidsAbove).foreach({case (e, m) =>
      //          val updated = e.copy(metadata = e.metadata.merge(m))
      //          state.graph -= updated
      //          state.graph += updated
      //      })
      // TODO: Fix API so we can save have the chosen
      state.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
      //      state.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId)).filterNot(e =>
      ////        chose.contains(e) || e.metadata.exists(m => uidsAbove.contains(m)))
      //        chose.contains(e))
    }
  }
}

object CaseSplitAction {
  val possibleSplitId = Identifier("possibleSplit")
  val splitTrue = Identifier("splitTrue")
  type SplitChooser = (RewriteSearchState, Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
    Set[HyperEdge[HyperTermId, HyperTermIdentifier]]

  def specificChooser(splitters: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]): SplitChooser = {
    (state: RewriteSearchState, chose: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) => {
      if (chose.length == splitters.length) Set.empty
      else splitters.take(chose.length + 1).toSet
    }
  }

  def shiftEdges(startId: Int, edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]])
  : Set[HyperEdge[HyperTermId, HyperTermIdentifier]] =
    edges.map(e => e.copy(target = e.target.copy(e.target.id + startId),
      sources = e.sources.map(hid => hid.copy(id = hid.id + startId))))

  def edgesMax(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): Int =
    edges.flatMap(e => e.sources :+ e.target).map(_.id).max

  def mergeNodes(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]], source: HyperTermId, target: HyperTermId)
  : Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    def switch(hyperTermId: HyperTermId) = if (hyperTermId == target) source else hyperTermId

    edges.map(e =>
      if (e.sources.contains(target) || e.target == target)
        e.copy(target = switch(e.target), sources = e.sources.map(switch))
      else e
    )
  }
}
