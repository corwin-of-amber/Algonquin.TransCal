package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.annotation.tailrec
import scala.collection.mutable


class ObservationalEquivalence(maxDepth: Int = 4) extends Action with LazyLogging {
  protected def createSearchAction(oPattern: Option[HyperPattern]): SearchAction = {
    new OperatorRunAction(maxDepth, oPattern.map(p => (r: RewriteSearchState) => r.graph.findSubgraph[Int](p).nonEmpty))
  }

  def getEquivesFromRewriteState(rewriteSearchState: RewriteSearchState, rewriteRules: Set[Operator[RewriteSearchState]]): (RewriteSearchState, Set[Set[HyperTermId]]) = {
    var addedAnchors = Set.empty[HyperEdge[HyperTermId, HyperTermIdentifier]]
    if (ObservationalEquivalence.getAnchors(rewriteSearchState).isEmpty) {
      logger.warn("Adding anchors to all nodes in observational equivalence")
      addedAnchors = rewriteSearchState.graph.nodes.map(n => ObservationalEquivalence.createAnchor(n))
      rewriteSearchState.graph ++= addedAnchors
    }

    val allAnchors = addedAnchors ++ ObservationalEquivalence.getAnchors(rewriteSearchState)
    val endPattern = ObservationalEquivalence.createEndPattern(allAnchors)

    val opAction = createSearchAction(Some(endPattern))
    val newState = opAction.fromRewriteState(rewriteSearchState, rewriteRules)

    val merged: Set[Set[HyperTermId]] = ObservationalEquivalence.getIdsToMerge(newState.graph.edges)

    rewriteSearchState.graph --= addedAnchors.flatMap(e => rewriteSearchState.graph.findByEdgeType(e.edgeType))
    newState.graph --= addedAnchors.flatMap(e => newState.graph.findByEdgeType(e.edgeType))
    (newState, merged)
  }

  def getEquives(actionSearchState: ActionSearchState): Set[Set[HyperTermId]] = {
    getEquivesFromRewriteState(new RewriteSearchState(actionSearchState.programs.hyperGraph), actionSearchState.rewriteRules)._2
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val rState = new RewriteSearchState(state.programs.hyperGraph)
    val equives = getEquivesFromRewriteState(rState, state.rewriteRules)._2.toSeq
    val newState = ObservationalEquivalence.mergeConclusions(rState, equives)
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }

  def fromTerms(terms: Seq[AnnotatedTree], rewriteRules: Set[Operator[RewriteSearchState]]): Set[Set[AnnotatedTree]] = {
    var maxId = -1
    val termToEdges = (for (t <- terms) yield {
      val (graph, root) = Programs.destructWithRoot(t, HyperTermId(maxId + 1))
      maxId = graph.nodes.map(_.id).max
      (t, (graph.edges + ObservationalEquivalence.createAnchor(root), root))
    }).toMap
    val idToTerm = termToEdges.map({case (term, (_, id)) => (id, term)})
    val allEdges = termToEdges.values.map(_._1).reduce((g1, g2) => g1 ++ g2)
    getEquivesFromRewriteState(new RewriteSearchState(new RewriteSearchState.HyperGraph(allEdges)), rewriteRules)._2
      .map(s => s.map(id => idToTerm(id)))
  }
}

object ObservationalEquivalence extends LazyLogging {
  val anchorStart = "Equiv_Anchor_For_"

  def createAnchor(hyperTermId: HyperTermId): HyperEdge[HyperTermId, HyperTermIdentifier] =
    HyperEdge(hyperTermId, HyperTermIdentifier(Identifier(s"$anchorStart${hyperTermId.id}")), Seq(), NonConstructableMetadata)

  def getAnchors(rewriteSearchState: RewriteSearchState): Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    rewriteSearchState.graph.edges.filter(_.edgeType.identifier.literal.startsWith(ObservationalEquivalence.anchorStart))
  }

  def createEndPattern(anchors: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): HyperPattern = {
    Programs.destructPattern(AnnotatedTree(Language.andCondBuilderId, anchors.map(a => AnnotatedTree.identifierOnly(a.edgeType.identifier)).toSeq, Seq.empty))
  }

  def getIdsToMerge(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): Set[Set[HyperTermId]] = {
    edges.filter(_.edgeType.identifier.literal.startsWith(ObservationalEquivalence.anchorStart))
      .groupBy(_.target).values.toSet
      .map((set:  Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
        set.map(e => HyperTermId(e.edgeType.identifier.literal.drop(ObservationalEquivalence.anchorStart.length).toInt)))
  }

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
    rState.graph --= rState.graph.edges.filter(_.edgeType.identifier.literal.startsWith("caseAnchor_"))
    rState
  }

  def flattenUnionConclusions[T](equives: Seq[Set[Set[T]]]): Set[Set[T]] = {
    if (equives isEmpty) Set.empty
    else {
      val unionFind = new mutable.UnionFind(equives.head.flatten.toSeq)
      for (eqGroups <- equives; eqGroup <- eqGroups if eqGroup.size > 1; a = eqGroup.head; b <- eqGroup.tail) {
        unionFind.union(a, b)
      }
      unionFind.sets
    }
  }

  @tailrec
  def flattenIntersectConclusions[T](equives: Seq[Set[Set[T]]]): Set[Set[T]] = {
    equives match {
      case Seq() => Set.empty
      case Seq(x) => x
      case s =>
        val tempRes = s(0).flatMap(s1 => s(1).map(s2 => s1.intersect(s2)))
        flattenIntersectConclusions(tempRes +: s.drop(2))
    }
  }
}