package synthesis.actions.operators

import structures.{IdMetadata, Uid}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.CaseSplitAction.SplitChooser
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator

class ObservationalEquivalenceWithCaseSplit(maxDepth: Int = 4,
                                            splitDepth: Option[Int] = None,
                                            chooser: Option[SplitChooser] = None)
  extends ObservationalEquivalence(maxDepth) {
  override protected def createSearchAction(oPattern: Option[HyperPattern]): SearchAction =
    new OperatorRunWithCaseSplit(maxDepth,
      oPattern.map(p => (r: RewriteSearchState) => r.graph.findSubgraph[Int](p).nonEmpty),
      splitDepth,
      chooser)
}