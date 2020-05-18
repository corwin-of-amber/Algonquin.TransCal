package synthesis.search.action.operators

import structures.immutable.HyperGraph
import structures.{HyperEdge, Metadata}
import synthesis.Programs.NonConstructableMetadata
import synthesis.search.action.operators.LocateAction.LocateMetadata
import synthesis.search.rewrite.operators.RewriteRule.HyperPattern
import synthesis.search.rewrite.operators.Template.{ExplicitTerm, TemplateTerm}
import synthesis.search.rewrite.operators.RewriteRule
import synthesis.search.action.ActionSearchState
import synthesis.search.rewrite.{NaiveSearch, RewriteSearchSpace, RewriteSearchState}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

/** Finding a hyperterm given a pattern. The given anchor will be added to the graph as a possible translation of the hyperterm.
  *
  * @author tomer
  * @since 11/18/18
  */
class LocateAction(anchor: HyperTermIdentifier, goal: HyperPattern, goalRoot: Option[TemplateTerm[HyperTermId]] = None, maxSearchDepth: Option[Int] = None) extends Action {
  /** To be used during the BFS rewrite search
    *
    * @param state the current state
    * @return is state final
    */
  protected def goalPredicate(state: RewriteSearchState): Boolean = state.graph.findEdges(anchor).nonEmpty

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
      HyperGraph(anchorPattern.mergeNodes(root, anchorRoot).map(e => e.copy(metadata = e.metadata.merge(NonConstructableMetadata))).toSeq: _*)
    }

    /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
    // Create new locator rule
    def locateDataCreator(idMap: Map[Int, HyperTermId], identMap: Map[Int, HyperTermIdentifier]): Metadata = {
      val hyperTermCreator: () => HyperTermId = {
        () => throw new RuntimeException("there should not be any leftover holes")
      }
      val newEdges = structures.generic.HyperGraph.fillPattern(goal, (idMap, identMap), hyperTermCreator)
      LocateMetadata(newEdges)
    }

    val locateRule = new RewriteRule(goal, destPattern, locateDataCreator)

    // Rewrite search
    val rewriteSearch = new NaiveSearch()
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(locateRule +: state.rewriteRules.toSeq, initialState, goalPredicate)
    val (rewriteResult, newState) = maxSearchDepth.map(d => rewriteSearch.search(spaceSearch, d)).getOrElse(rewriteSearch.search(spaceSearch))

    // Process result
    val newEdges = newState.graph.findEdges(anchor).take(1)
    val newPrograms =
      if (rewriteResult) {
        val tempProgs = Programs(newState.graph)
        val terms = tempProgs.reconstructWithPattern(newEdges.head.target, goal, goalRoot)
        if (terms.nonEmpty) logger.debug(terms.head.toString)
        else logger.debug("Found term not constructable (probably a symbol)")
        tempProgs
      } else {
        logger.warn("Locate did not find the requested pattern.")
        Programs(state.programs.hyperGraph.++(newEdges))
      }
    ActionSearchState(newPrograms, state.rewriteRules)
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