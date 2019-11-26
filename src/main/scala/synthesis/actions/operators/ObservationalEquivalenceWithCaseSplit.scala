package synthesis.actions.operators

import synthesis.actions.operators.CaseSplitAction.SplitChooser
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState

class ObservationalEquivalenceWithCaseSplit(maxDepth: Int = 4,
                                            splitDepth: Option[Int] = None,
                                            chooser: Option[SplitChooser] = None)
  extends ObservationalEquivalence(maxDepth) {
  override protected def createSearchAction(oPattern: Option[HyperPattern]): SearchAction =
    new OperatorRunWithCaseSplit(maxDepth,
      oPattern.map(p => (r: RewriteSearchState) => r.graph.findSubgraph[Int](p).nonEmpty),
      splitDepth = splitDepth,
      chooser = chooser)
}