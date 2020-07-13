package synthesis.search.actions

import structures.{HyperEdge, Metadata}
import synthesis.Programs.NonConstructableMetadata
import synthesis.search.{ActionSearchState, NaiveSearch}
import LocateAction.LocateMetadata
import structures.generic.HyperGraph.Match
import synthesis.search.rewrites.PatternRewriteRule
import synthesis.search.rewrites.PatternRewriteRule.HyperPattern
import synthesis.search.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

/** Finding a hyperterm given a pattern. The given anchor will be added to the graph as a possible translation of the hyperterm.
  *
  * @author tomer
  * @since 11/18/18
  */
class LocateAction(anchor: HyperTermIdentifier, goal: HyperPattern, goalRoot: Option[TemplateTerm[HyperTermId]] = None, maxSearchDepth: Option[Int] = Some(20)) extends Action {
  /** To be used during the BFS rewrite search
    *
    * @param graph the current state
    * @return is state final
    */
  protected def goalPredicate(graph: structures.generic.HyperGraph[HyperTermId, HyperTermIdentifier]): Boolean =
    graph.findEdges(anchor).nonEmpty

  override def apply(state: ActionSearchState): ActionSearchState = {
    // Anchor should not have a type as it will add a type annotation to the node we are marking
    require(anchor.identifier.annotation.isEmpty)
    // We assume only one root as it is a pattern from user.
    logger.debug(s"Running Locate with $anchor")
    val roots = {
      val allTargets = goal.targets
      val nonTypeSources = goal.edges.filter(_.edgeType != ExplicitTerm(HyperTermIdentifier(Language.typeId))).flatMap(_.sources)
      val tempRoots = allTargets.diff(nonTypeSources)
      val typeRoots = goal.findByEdgeType(ExplicitTerm(HyperTermIdentifier(Language.typeId))).flatMap(x => Set(x.sources(1), x.target))
      tempRoots.diff(typeRoots)
    }
    assert(roots.size == 1)
    //HyperGraphManyWithOrderToOne(HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](root, ExplicitTerm(anchor), Seq.empty, NonConstructableMetadata))
    val destPattern = {
      val root = roots.head
      val (anchorPattern, anchorRoot) = Programs.destructPatternsWithRoots(Seq(AnnotatedTree.identifierOnly(anchor.identifier))).head
      structures.generic.HyperGraph(anchorPattern.mergeNodes(root, anchorRoot).map(e => e.copy(metadata = e.metadata.merge(NonConstructableMetadata))).toSeq: _*)
    }

    /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
    // Create new locator rule
    def locateDataCreator(matched: Match[HyperTermId, HyperTermIdentifier, Int]): Metadata = {
      val hyperTermCreator: () => HyperTermId = {
        () => throw new RuntimeException("there should not be any leftover holes")
      }
      val newEdges = structures.generic.HyperGraph.fillPattern(goal, matched, hyperTermCreator)
      LocateMetadata(newEdges)
    }

    val locateRule = new PatternRewriteRule(goal, destPattern).registerMetadataCreator(locateDataCreator)

    // Rewrite search
    val rewriteSearch = new NaiveSearch(isGoal = goalPredicate)
    state.addRule(locateRule)
    // TODO: enable search depth in search actions
    val newState = rewriteSearch(state, maxSearchDepth.map(_.toDouble))
    val rewriteResult = goalPredicate(newState.programs.queryGraph)
    newState.removeRule(locateRule)

    // Process result
    val newEdges = newState.programs.queryGraph.findEdges(anchor).take(1)
    if (rewriteResult) {
      val terms = newState.programs.reconstructWithPattern(newEdges.head.target, goal, goalRoot)
      if (terms.nonEmpty) logger.debug(terms.head.toString)
      else logger.debug("Found term not constructable (probably a symbol)")
    } else {
      logger.warn("Locate did not find the requested pattern.")
    }
    newState
  }
}

object LocateAction {
  val createTemporaryAnchor: () => HyperTermIdentifier = {
    val anchors = Stream.from(0).map(i =>
      HyperTermIdentifier(Identifier(s"temp_anchor_$i"))
    ).iterator
    anchors.next
  }

  case class LocateMetadata(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) extends Metadata {
    override def toStr: String = s"LocateMetadata($edges)"
  }

}