package synthesis.actions.operators

import structures.HyperEdge
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}


object ObservationalEquivalence {
  def getEquives(rewriteRules: Set[Operator[RewriteSearchState]], terms: Seq[AnnotatedTree]): Set[Set[AnnotatedTree]] = {
    val (allInOne, root) = Programs.destructWithRoot(AnnotatedTree.withoutAnnotations(Language.semicolonId, terms.toList))
    val top = allInOne.edges.find(e => e.target == root && e.edgeType == HyperTermIdentifier(Language.semicolonId)).head
    val anchorToTerm = top.sources.zipWithIndex.map(tAndI => (HyperEdge(tAndI._1, HyperTermIdentifier(Identifier(s"${terms(tAndI._2)}")), List.empty, NonConstructableMetadata), terms(tAndI._2))).toMap
    val anchors = anchorToTerm.keys.toSet
    val searchGraph = allInOne.addEdges(anchors)
    var rewriteState = new RewriteSearchState(searchGraph)
    for (i <- 1 to 10; op <- rewriteRules) rewriteState = op(rewriteState)
    val termToTarget: Map[AnnotatedTree, HyperTermId] = anchorToTerm.map(t => (t._2, rewriteState.graph.findEdges(t._1.edgeType).head.target))
    termToTarget.groupBy(t => t._2).map(_._2.keys.toSet).toSet
  }
}