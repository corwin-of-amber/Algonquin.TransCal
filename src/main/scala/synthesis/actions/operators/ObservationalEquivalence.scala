package synthesis.actions.operators

import structures.HyperEdge
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}


class ObservationalEquivalence(maxDepth: Int = 4) extends Action {
  def getEquives(actionSearchState: ActionSearchState): Set[Set[HyperTermId]] = {
    val allAnchors = actionSearchState.programs.hyperGraph.filter(_.edgeType.identifier.literal.startsWith(ObservationalEquivalence.anchorStart))
    val endPattern = Programs.destructPattern(AnnotatedTree(Language.andCondBuilderId, allAnchors.map(a => AnnotatedTree.identifierOnly(a.edgeType.identifier)).toSeq, Seq.empty))
    val opAction = new OperatorRunAction(maxDepth, Some((r: RewriteSearchState) => r.graph.findSubgraph[Int](endPattern).nonEmpty))
    val newState = opAction(actionSearchState)
    val merged: Set[Set[HyperTermId]] = newState.programs.hyperGraph.edges
      .filter(_.edgeType.identifier.literal.startsWith(ObservationalEquivalence.anchorStart))
      .groupBy(_.target).values.toSet
      .map((set:  Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
        set.map(e => HyperTermId(e.edgeType.identifier.literal.drop(ObservationalEquivalence.anchorStart.length).toInt)))
    merged
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val equives = getEquives(state)
    val rewriteSearchState = new RewriteSearchState(state.programs.hyperGraph)
    for (s <- equives; first = s.head; next <- s.tail) {
      rewriteSearchState.graph.mergeNodes(first, next)
    }
    ActionSearchState(Programs(rewriteSearchState.graph), state.rewriteRules)
  }

  def fromTerms(terms: Seq[AnnotatedTree], rewriteRules: Set[Operator[RewriteSearchState]]): Set[Set[AnnotatedTree]] = {
    var maxId = -1
    val termToGraph = (for (t <- terms) yield {
      val (graph, root) = Programs.destructWithRoot(t, HyperTermId(maxId + 1))
      maxId = graph.nodes.map(_.id).max
      (t, (graph + ObservationalEquivalence.createAnchor(root), root))
    }).toMap
    val idToTerm = termToGraph.map({case (term, (_, id)) => (id, term)})
    val fullGraph = termToGraph.values.map(_._1).reduce((g1, g2) => g1 ++ g2)
    getEquives(ActionSearchState(Programs(fullGraph), rewriteRules))
      .map(s => s.map(id => idToTerm(id)))
  }
}

object ObservationalEquivalence {
  val anchorStart = "Equiv_Anchor_For_"

  def createAnchor(hyperTermId: HyperTermId): HyperEdge[HyperTermId, HyperTermIdentifier] =
    HyperEdge(hyperTermId, HyperTermIdentifier(Identifier(s"$anchorStart${hyperTermId.id}")), Seq(), NonConstructableMetadata)
}