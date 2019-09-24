package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator
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

  def getFoundConclusionsFromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]]): Set[Set[HyperTermId]] = {
    val cleanedAndWithAnchors = (state.graph ++ state.graph.map(e => ObservationalEquivalence.createAnchor(e.target)))
    val equives = splitter.sources.tail.map(s => {
      val newGraph: RewriteSearchState.HyperGraph = {
        // merge nodes clones the graph
        val temp = cleanedAndWithAnchors.mergeNodes(s, splitter.sources.head)
        temp --= temp.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
      }
      val (tempState, res) = obvEquiv.getEquivesFromRewriteState(RewriteSearchState(newGraph), rules)
      res
    })

    val toMerge = equives.head.filter(_.size > 1).flatMap(hyperTermIds => {
      val restEquives = equives.tail
      for(i <- hyperTermIds; relevantSets = restEquives.map(_.find(_.contains(i)).get)) yield {
        relevantSets.fold(hyperTermIds)((distjuinted, set) => distjuinted.intersect(set))
      }
    }).filter(_.size > 1)
    toMerge
  }

  def getFoundConclusions(state: ActionSearchState): Set[Set[HyperTermId]] = {
    getFoundConclusionsFromRewriteState(new RewriteSearchState(state.programs.hyperGraph), state.rewriteRules)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val rState = new RewriteSearchState(state.programs.hyperGraph)
    val toMerge = getFoundConclusionsFromRewriteState(rState, state.rewriteRules).toSeq
    val newState = ObservationalEquivalence.mergeConclusions(rState, toMerge)
    ActionSearchState(Programs(newState.graph.filterNot(_.edgeType.identifier.literal.startsWith("caseAnchor"))), state.rewriteRules)
  }
}

object CaseSplitAction {
  val possibleSplitId = Identifier("possibleSplit")
  val splitTrue = Identifier("splitTrue")
}
