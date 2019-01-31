package synthesis.actions.operators

import structures.immutable.HyperGraphManyWithOrderToOne
import structures.{EmptyMetadata, HyperEdge, Metadata}
import syntax.Identifier
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LocateAction.LocateMetadata
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.rewrites.{RewriteRule, RewriteSearchSpace, RewriteSearchState}
import synthesis.search.{DepthFirstSearch, IterativeDeepening}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/** Finding a hyperterm given a pattern. The given anchor will be added to the graph as a possible translation of the hyperterm.
  * @author tomer
  * @since 11/18/18
  */
class LocateAction(anchor: HyperTermIdentifier, goal: HyperPattern, root: TemplateTerm[HyperTermId]) extends Action {
  /** To be used during the BFS rewrite search
    *
    * @param state the current state
    * @return is state final
    */
  protected def goalPredicate(state: RewriteSearchState): Boolean = state.graph.findEdges(anchor).nonEmpty

  override def apply(state: ActionSearchState): ActionSearchState = {
    // We assume only one root as it is a pattern from user.
    val roots = goal.edges.map(_.target) diff goal.edges.flatMap(_.sources)
    assert(roots.size == 1)
    val root = roots.head
    val destPattern = HyperGraphManyWithOrderToOne(HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](root, ExplicitTerm(anchor), Seq.empty, NonConstructableMetadata))

    /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
    // Create new locator rule
    def locateDataCreator(hyperIdMap: Map[Int, HyperTermId], hyperIdent: Map[Int, HyperTermIdentifier]): Metadata = {
      def extract1(t: TemplateTerm[HyperTermId]) = t match {
        case ReferenceTerm(i) => hyperIdMap(i)
        case ExplicitTerm(ht) => ht
      }
      def extract2(t: TemplateTerm[HyperTermIdentifier]) = t match {
        case ReferenceTerm(i) => hyperIdent(i)
        case ExplicitTerm(ht) => ht
      }
      val newEdges = goal.edges.map({
        case HyperEdge(t, et, sources, meta) => HyperEdge[HyperTermId, HyperTermIdentifier](extract1(t), extract2(et), sources.map(extract1), meta)
      })
      LocateMetadata(newEdges)
    }

    val locateRule = new RewriteRule(goal, destPattern, locateDataCreator)

    // Rewrite search
    val rewriteSearch = new IterativeDeepening(new DepthFirstSearch[RewriteSearchState, RewriteSearchSpace]())
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq :+ locateRule, initialState, goalPredicate)
    val rewriteResult = rewriteSearch.search(spaceSearch)

    // Process result
    val newEdges = rewriteResult.map(_.graph.findEdges(anchor)).toSet.flatten.take(1)
    val newPrograms = Programs(state.programs.hyperGraph.addEdges(newEdges))
    if (newEdges.isEmpty) logger.warn("Locate did not find the requested pattern.")
    else {
      val terms = newPrograms.reconstructWithPattern(newEdges.head.target, goal, root)
      if (terms.hasNext) logger.debug(terms.next().toString())
      else logger.debug("Found term not constructable (probably a symbol)")
    }
    ActionSearchState(newPrograms, state.rewriteRules)
  }
}

object LocateAction {
  val createTemporaryAnchor: () => HyperTermIdentifier = {
    val anchors = Stream.from(0).map( i =>
      HyperTermIdentifier(new Identifier(s"temp anchor $i"))
    ).toIterator
    anchors.next
  }

  case class LocateMetadata(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) extends Metadata {
    override def toStr: String = s"LocateMetadata($edges)"
  }
}