package synthesis.search.actions

import synthesis.search.RewriteSearchState
import CaseSplitAction.SplitChooser
import synthesis.search.rewrites.RewriteRule.HyperPattern

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