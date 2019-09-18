package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import structures.generic.HyperGraph.HyperGraphPattern
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{Identifier, Language}


/** Case split action splits a variable into options specified by the splitter edge. Variable should not have edges.
  * The graph is duplicated and observational equivalence is run on each new graph.
  * The results are than matched and nodes that should merge will merge in the result state.
  *
  * Input graph should contain an edge of type possibleSplit where the target is SplitTrue and the first source is the
  * variable to change and possible values are the rest of the sources.
  */
class CaseSplitAction(splitter: HyperEdge[HyperTermId, HyperTermIdentifier]) extends Action {
  val obvEquiv = new ObservationalEquivalence(maxDepth = 4)

  def getFoundConclusions(state: ActionSearchState): Set[Set[HyperTermId]] = {
    val cleanedAndWithAnchors = (state.programs.hyperGraph ++ state.programs.hyperGraph.map(e => ObservationalEquivalence.createAnchor(e.target))).
      filterNot(_.target == splitter.sources.head)
    val equives = splitter.sources.tail.map(s => {
      val newGraph = {
        val temp = cleanedAndWithAnchors.mergeNodes(s, splitter.sources.head)
        temp -- temp.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
      }
      obvEquiv.getEquives(ActionSearchState(Programs(newGraph), state.rewriteRules))
    })

    val toMerge = equives.head.filter(_.size > 1).flatMap(hyperTermIds => {
      val restEquives = equives.tail
      for(i <- hyperTermIds; relevantSets = restEquives.map(_.find(_.contains(i)).get)) yield {
        relevantSets.fold(hyperTermIds)((distjuinted, set) => distjuinted.intersect(set))
      }
    }).filter(_.size > 1)
    toMerge
  }

  private def createAnchor(hyperTermId: HyperTermId, index: Int) =
    HyperEdge(hyperTermId, HyperTermIdentifier(Identifier(s"caseAnchor_$index")), Seq.empty, EmptyMetadata)

  override def apply(state: ActionSearchState): ActionSearchState = {
    val toMerge = getFoundConclusions(state).toSeq

    val rState = new RewriteSearchState(state.programs.hyperGraph)
    rState.graph ++= toMerge.zipWithIndex.flatMap({ case (ids, i) => ids.map(id => createAnchor(id, i)) })
    for (index <- toMerge.indices) {
      while (rState.graph.findByEdgeType(createAnchor(HyperTermId(0), index).edgeType).size > 1) {
        val set = rState.graph.findByEdgeType(createAnchor(HyperTermId(0), index).edgeType)
        rState.graph.mergeNodes(set.head.target, set.last.target)
      }
    }
    ActionSearchState(Programs(rState.graph.filterNot(_.edgeType.identifier.literal.startsWith("caseAnchor"))), state.rewriteRules)
  }
}

object CaseSplitAction {
  val possibleSplitId = Identifier("possibleSplit")
  val splitTrue = Identifier("splitTrue")
}
