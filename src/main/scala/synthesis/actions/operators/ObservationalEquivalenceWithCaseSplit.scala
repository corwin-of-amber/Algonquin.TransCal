package synthesis.actions.operators

import structures.{IdMetadata, Uid}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator

class ObservationalEquivalenceWithCaseSplit(maxDepth: Int = 4) extends ObservationalEquivalence(maxDepth) {
  override protected def createSearchAction(oPattern: Option[HyperPattern]): SearchAction =
    new OperatorRunWithCaseSplit(maxDepth, oPattern.map(p => (r: RewriteSearchState) => r.graph.findSubgraph[Int](p).nonEmpty))
}