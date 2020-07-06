package synthesis.search.actions

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.search.rewrites.RewriteRule
import synthesis.search.rewrites.PatternRewriteRule.HyperPattern
import synthesis.search.{ActionSearchState, Operator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.annotation.tailrec
import scala.collection.mutable


class ObservationalEquivalence(maxDepth: Int = 4, searchAction: Option[Action] = None) extends Action with LazyLogging {
  // TODO: Make search action a must
  // TODO: add end pattern api to search action
  val uniquePrefix: String = UUID.randomUUID().toString

  protected def createSearchAction(oPattern: Option[HyperPattern]): Action = {
    new OperatorRunAction(maxDepth, oPattern.map(p => (r: RewriteRule.HyperGraph) => r.findSubgraph[Int](p).nonEmpty))
  }

  def getEquives(actionSearchState: ActionSearchState): (ActionSearchState, Set[Set[HyperTermId]]) = {
    val allAnchors = actionSearchState.programs.queryGraph.nodes.map(n => ObservationalEquivalence.createAnchor(uniquePrefix, n))
    actionSearchState.updateGraph(graph => graph ++= allAnchors)
    val opAction =
      searchAction.getOrElse(createSearchAction(Some(ObservationalEquivalence.createEndPattern(allAnchors))))
    val newState = opAction(actionSearchState)
    val merged: Set[Set[HyperTermId]] = ObservationalEquivalence.getIdsToMerge(uniquePrefix, newState.programs.queryGraph.edges)
    val toRemove = allAnchors.flatMap(e => actionSearchState.programs.queryGraph.findByEdgeType(e.edgeType))
    actionSearchState.updateGraph(graph => graph --= toRemove)
    (newState, merged)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val equives = getEquives(state)._2.toSeq
    val newState = ObservationalEquivalence.mergeConclusions(state, equives)
    newState
  }

  def fromTerms(terms: Seq[AnnotatedTree], rewriteRules: Set[RewriteRule]): Set[Set[AnnotatedTree]] = {
    // TODO: fix id to term to be able to deal with moving term targets
    // TODO: insert common subterms once
    // TODO: use programs terms constructor (more efficient)
    val graph = structures.mutable.CompactHyperGraph.empty[HyperTermId, HyperTermIdentifier]
    terms.foreach(t => {
      graph ++= Programs.destruct(t, if (graph.nonEmpty) graph.nodes.maxBy(_.id) else HyperTermId(0))
    })
    val termToEdges = (for (t <- terms) yield {
      val (pattern, root) = Programs.destructPatternWithRoot(t)
      (t, graph.findSubgraph[Int](pattern).head.nodeMap(root.id))
    }).toMap
    val idToTerm = termToEdges.map({case (term, id) => (id, term)})
    val res = getEquives(new ActionSearchState(graph, rewriteRules))
    res._2.filter(_.exists(idToTerm.contains)).map(s => s.filter(idToTerm.contains).map(id => idToTerm(id)))
  }
}

object ObservationalEquivalence extends LazyLogging {
  private val anchorStart = "Equiv_Anchor_For_"

  def createAnchor(prefix: String, hyperTermId: HyperTermId): HyperEdge[HyperTermId, HyperTermIdentifier] =
    HyperEdge(hyperTermId, HyperTermIdentifier(Identifier(s"$prefix$anchorStart${hyperTermId.id}")), Seq(), NonConstructableMetadata)

  def getAnchors(prefix: String, state: ActionSearchState): Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    state.programs.queryGraph.edges.filter(_.edgeType.identifier.literal.startsWith(prefix+ObservationalEquivalence.anchorStart))
  }

  def createEndPattern(anchors: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): HyperPattern = {
    Programs.destructPattern(AnnotatedTree(Language.andCondBuilderId, anchors.map(a => AnnotatedTree.identifierOnly(a.edgeType.identifier)).toSeq, Seq.empty))
  }

  def getIdsToMerge(prefix: String, edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): Set[Set[HyperTermId]] = {
    edges.filter(_.edgeType.identifier.literal.startsWith(prefix + ObservationalEquivalence.anchorStart))
      .groupBy(_.target).values.toSet
      .map((set:  Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
        set.map(e => HyperTermId(e.edgeType.identifier.literal.drop(prefix.length + ObservationalEquivalence.anchorStart.length).toInt)))
  }

  def mergeConclusions(state: ActionSearchState, equives: Seq[Set[HyperTermId]]): ActionSearchState = {
    def createAnchor(hyperTermId: HyperTermId, index: Int) =
      HyperEdge(hyperTermId, HyperTermIdentifier(Identifier(s"caseAnchor_$index")), Seq.empty, EmptyMetadata)

    state.updateGraph(graph => {
      graph ++= equives.zipWithIndex.flatMap({ case (ids, i) => ids.map(id => createAnchor(id, i)) })
      for (index <- equives.indices) {
        while (graph.findByEdgeType(createAnchor(HyperTermId(0), index).edgeType).size > 1) {
          val set = graph.findByEdgeType(createAnchor(HyperTermId(0), index).edgeType)
          graph.mergeNodesInPlace(set.head.target, set.last.target)
        }
      }
      graph --= graph.edges.filter(_.edgeType.identifier.literal.startsWith("caseAnchor_"))
    })

    state
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