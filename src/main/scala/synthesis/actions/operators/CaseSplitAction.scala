package synthesis.actions.operators

import structures.HyperEdge
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
class CaseSplitAction(splitDepth: Int,
                      splitterChooser: Option[(RewriteSearchState, Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
                        Set[HyperEdge[HyperTermId, HyperTermIdentifier]]] = None,
                      maxDepth: Int = 4) extends Action {
  val obvEquiv = new ObservationalEquivalence(maxDepth = maxDepth)

  def getFoundConclusionsFromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]])
  : Set[Set[HyperTermId]] = innerGetFoundConclusionsFromRewriteState(state, rules, Seq.empty)

  private def innerGetFoundConclusionsFromRewriteState(state: RewriteSearchState,
                                                       rules: Set[Operator[RewriteSearchState]],
                                                       chosen: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]])
  : Set[Set[HyperTermId]] = {
    val chooser = splitterChooser.getOrElse(randomChooser)
    val withAnchors =
      if (state.graph.edgeTypes.exists(_.identifier.literal.startsWith(anchorStart))) state.graph
      else state.graph ++ state.graph.map(e => createAnchor(e.target))
    val splitters = chooser(state, chosen)
    val results = for (splitter <- splitters) yield {
      val tupledGraph = {
        // Need to shift all edges on graph because we want to prevent one changed value from affecting parts of other
        // values. Working on edges to prevent unwanted compaction until adding back anchors and adding new tuples.
        val initialEdges = withAnchors.edges.filter(e => (!e.edgeType.identifier.literal.startsWith(anchorStart))
          && e.target != splitter.sources.head)
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
          Set(e.copy(target = newId),
            HyperEdge(newId,
              HyperTermIdentifier(Language.tupleId),
              shiftedEdgesWithShiftSize.map({ case (_, shift) =>
                HyperTermId(e.target.id + shift)
              }),
              NonConstructableMetadata)
          )
        })
        new RewriteSearchState.HyperGraph(shiftedEdgesWithShiftSize.flatMap(_._1).toSet ++ tuplesAndAnchors)
      }

      // Observational Equivalence will use anchors in graph and calculate hyper term id from their identifiers.
      if (chosen.length + 1 < splitDepth)
        innerGetFoundConclusionsFromRewriteState(RewriteSearchState(tupledGraph), rules, chosen :+ splitter)
      else
        obvEquiv.getEquivesFromRewriteState(RewriteSearchState(tupledGraph), rules)._2
    }
  }

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

  def randomChooser(state: RewriteSearchState, chose: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]])
  : Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {

  }
}

object CaseSplitAction {
  val possibleSplitId = Identifier("possibleSplit")
  val splitTrue = Identifier("splitTrue")

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
