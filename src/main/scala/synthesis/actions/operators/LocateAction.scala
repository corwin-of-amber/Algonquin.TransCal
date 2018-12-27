package synthesis.actions.operators

import structures.{EmptyMetadata, HyperEdge, Metadata}
import structures.immutable.HyperGraphManyWithOrderToOne
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LocateAction.LocateMetadata
import synthesis.rewrites.RewriteRule.{Category, HyperPattern, RewriteRuleMetadata}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.rewrites.{RewriteRule, RewriteSearchSpace, RewriteSearchState}
import synthesis.search.BreadthFirstSearch

/** Finding a hyperterm given a pattern. The given anchor will be added to the graph as a possible translation of the hyperterm.
  * @author tomer
  * @since 11/18/18
  */
class LocateAction(anchor: HyperTermIdentifier, goal: HyperPattern) extends Action {
  assert(anchor.identifier.kind == Programs.Kinds.NonConstructable.toString)

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
    val destPattern = HyperGraphManyWithOrderToOne(Set(HyperEdge[TemplateTerm, TemplateTerm](root, ExplicitTerm(anchor), Seq.empty, EmptyMetadata)))

    /** Locate using a rewrite search until we use the new rewrite rule. Add the new edge to the new state. */
    // Create new locator rule
    def locateDataCreator(referenceMap: Map[Int, Either[HyperTermId, HyperTermIdentifier]]): Metadata = {
      def extract[T](t: TemplateTerm): T = t match {
        case ReferenceTerm(i) => referenceMap(i) match {case Left(a) => a; case Right(b) => b} asInstanceOf[T]
        case ExplicitTerm(ht) => ht.asInstanceOf[T]
      }

      val newEdges = goal.edges.map({
        case HyperEdge(t, et, sources, meta) => HyperEdge[HyperTermId, HyperTermIdentifier](extract(t), extract(et), sources.map(extract), meta)
      })
      LocateMetadata(newEdges)
    }

    val locateRule = new RewriteRule(goal, destPattern, locateDataCreator)

    // Rewrite search
    val rewriteSearch = new BreadthFirstSearch[RewriteSearchState, RewriteSearchSpace]
    val initialState = new RewriteSearchState(state.programs.hyperGraph)
    val spaceSearch = new RewriteSearchSpace(state.rewriteRules.toSeq :+ locateRule, initialState, goalPredicate)
    val rewriteResult = rewriteSearch.search(spaceSearch)

    // Process result
    val newEdges = rewriteResult.map(_.graph.findEdges(anchor)).toSet.flatten.take(1)
    logFoundEdges(state, newEdges)
    new ActionSearchState(Programs(state.programs.hyperGraph.addEdges(newEdges)), state.rewriteRules)
  }

  private def logFoundEdges(state: ActionSearchState, newEdges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): Unit = {
    val foundEdges = newEdges.headOption.map(_.metadata.find(_.isInstanceOf[LocateMetadata])
      .map(_.asInstanceOf[LocateMetadata].edges).getOrElse(Set.empty)
    ).getOrElse(Set.empty)
    val filterTargets = foundEdges.map(_.target)
    val reconstructGraph = HyperGraphManyWithOrderToOne[HyperTermId, HyperTermIdentifier](
      state.programs.hyperGraph.edges.filterNot(e => filterTargets.contains(e.target)) ++ foundEdges
    )
    if (newEdges.isEmpty) logger.warn("Locate did not find the requested pattern.")
    else logger.info(Programs(reconstructGraph).reconstruct(newEdges.head.target).next().toString())
  }
}

object LocateAction {
  case class LocateMetadata(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) extends Metadata {
    override def toStr: String = s"LocateMetadata($edges)"
  }
}