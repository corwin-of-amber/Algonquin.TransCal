package synthesis.search.actions

import CaseSplitAction.SplitChooser
import synthesis.{HyperTermId, HyperTermIdentifier}
import synthesis.search.rewrites.RewriteRule.HyperPattern

class ObservationalEquivalenceWithCaseSplit(maxDepth: Int = 4,
                                            splitDepth: Option[Int] = None,
                                            chooser: Option[SplitChooser] = None)
  extends ObservationalEquivalence(maxDepth) {
  override protected def createSearchAction(oPattern: Option[HyperPattern]): Action =
    new OperatorRunWithCaseSplit(maxDepth,
      oPattern.map(p => (r: structures.generic.HyperGraph[HyperTermId, HyperTermIdentifier]) => r.findSubgraph[Int](p).nonEmpty),
      splitDepth = splitDepth,
      chooser = chooser)
}