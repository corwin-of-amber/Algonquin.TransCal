package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}


class ObservationalEquivalence(maxDepth: Int = 4) extends Action {
  def getEquivesFromRewriteState(rewriteSearchState: RewriteSearchState, rewriteRules: Set[Operator[RewriteSearchState]]): Set[Set[HyperTermId]] = {
    val allAnchors = rewriteSearchState.graph.filter(_.edgeType.identifier.literal.startsWith(ObservationalEquivalence.anchorStart))
    val endPattern = Programs.destructPattern(AnnotatedTree(Language.andCondBuilderId, allAnchors.map(a => AnnotatedTree.identifierOnly(a.edgeType.identifier)).toSeq, Seq.empty))
    val opAction = new OperatorRunAction(maxDepth, Some((r: RewriteSearchState) => r.graph.findSubgraph[Int](endPattern).nonEmpty))
    val newState = opAction.fromRewriteState(rewriteSearchState, rewriteRules.toSeq)
    val merged: Set[Set[HyperTermId]] = newState.graph.edges
      .filter(_.edgeType.identifier.literal.startsWith(ObservationalEquivalence.anchorStart))
      .groupBy(_.target).values.toSet
      .map((set:  Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
        set.map(e => HyperTermId(e.edgeType.identifier.literal.drop(ObservationalEquivalence.anchorStart.length).toInt)))
    merged
  }

  def getEquives(actionSearchState: ActionSearchState): Set[Set[HyperTermId]] = {
    getEquivesFromRewriteState(new RewriteSearchState(actionSearchState.programs.hyperGraph), actionSearchState.rewriteRules)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val rState = new RewriteSearchState(state.programs.hyperGraph)
    val equives = getEquivesFromRewriteState(rState, state.rewriteRules).toSeq
    val newState = ObservationalEquivalence.mergeConclusions(rState, equives)
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
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

  def mergeConclusions(rState: RewriteSearchState, equives: Seq[Set[HyperTermId]]): RewriteSearchState = {
    def createAnchor(hyperTermId: HyperTermId, index: Int) =
      HyperEdge(hyperTermId, HyperTermIdentifier(Identifier(s"caseAnchor_$index")), Seq.empty, EmptyMetadata)

    rState.graph ++= equives.zipWithIndex.flatMap({ case (ids, i) => ids.map(id => createAnchor(id, i)) })
    for (index <- equives.indices) {
      while (rState.graph.findByEdgeType(createAnchor(HyperTermId(0), index).edgeType).size > 1) {
        val set = rState.graph.findByEdgeType(createAnchor(HyperTermId(0), index).edgeType)
        rState.graph.mergeNodesInPlace(set.head.target, set.last.target)
      }
    }
    rState
  }
}