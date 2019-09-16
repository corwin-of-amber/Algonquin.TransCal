package synthesis.actions.operators

import synthesis.actions.ActionSearchState
import synthesis.{HyperTermIdentifier, Programs}
import transcallang.Identifier


/** Case split action splits a variable into options specified by the splitter edge. Variable should not have edges.
  * The graph is duplicated and observational equivalence is run on each new graph.
  * The results are than matched and nodes that should merge will merge in the result state.
  *
  * Input graph should contain an edge of type possibleSplit where the target is SplitTrue and the first source is the
  * variable to change and possible values are the rest of the sources.
  */
class CaseSplitAction extends Action {
  val obvEquiv = new ObservationalEquivalence()

  override def apply(state: ActionSearchState): ActionSearchState = {
    val splitEdges = state.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
    assert(splitEdges.nonEmpty)
    val splitter = splitEdges.head
    // Find the replacee
    // remove all edges to prevent compaction? or verify no nonempty edges
    // removing edges can cause huge problems because they can be recreated.
    // We want to work on vars that we dont know stuff about so I am going on empty edges only
    assert(state.programs.hyperGraph.findByTarget(splitter.sources.head).forall(e => e.sources.isEmpty))

    val cleanedAndWithAnchors = (state.programs.hyperGraph ++ state.programs.hyperGraph.map(e => ObservationalEquivalence.createAnchor(e.target))).
      filterNot(_.target == splitter.sources.head)
    val equives = splitter.sources.tail.map(s => {
      val newGraph = cleanedAndWithAnchors.mergeNodes(s, splitter.sources.head)
      obvEquiv.getEquives(ActionSearchState(Programs(newGraph), state.rewriteRules))
    })

    val toMerge = equives.head.filter(_.size > 1).flatMap(hyperTermIds => {
      val restEquives = equives.tail
      for(i <- hyperTermIds; relevantSets = restEquives.map(_.find(_.contains(i)).get)) yield {
        relevantSets.fold(hyperTermIds)((distjuinted, set) => distjuinted.intersect(set))
      }
    }).filter(_.size > 1)

    ActionSearchState(Programs(toMerge.foldLeft(state.programs.hyperGraph)((graph, set) => {
      set.tail.foldLeft(graph)((g, newId) => g.mergeNodes(set.head, newId))
    })), state.rewriteRules)
  }
}

object CaseSplitAction {
  val possibleSplitId = Identifier("possibleSplit")
  val splitTrue = Identifier("splitTrue")
}
