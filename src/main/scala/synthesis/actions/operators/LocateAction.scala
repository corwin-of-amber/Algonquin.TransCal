package synthesis.actions.operators

import relentless.rewriting.{RevisionDiff, Rules}
import relentless.rewriting.RuleBasedTactic.{Markers, ⇢}
import syntax.{Identifier, Scheme, Tree}
import syntax.Scheme.Arity
import synthesis.HyperTermIdentifier
import synthesis.actions.ActionSearchState

/**
  * @author tomer
  * @since 11/18/18
  */
class LocateAction(anchor: HyperTermIdentifier, scheme: Scheme with Arity) extends Action {
  override def apply(state: ActionSearchState): ActionSearchState = {
    /** Hyperterms representing the anchor (ideally, there should be exactly one) */
    val anchor_# = state.programs.hyperGraph.findEdges(anchor) map (_.target)
    /** All placeholder hyperedges where first parameter is in anchor_# */
    val matches = state.programs.hyperGraph.findEdges(LocateAction.placeholderEx) filter (anchor_# contains _(2))

    // choose the first term for each match for the scheme parameters
    val allAlternatives =
      for (edge <- matches) yield {
        val components = edge.sources.drop(1)  // (the first parameter would be the marker)
        assert(components.length == scheme.arity)
        val alts = for (root <- components) yield state.programs.reconstruct(root)
        alts
      }
    allAlternatives.headOption match {
      case None => state
      case Some(h) =>
        val selected = h map (_.next())
//          Finds equivs (?????)
        val equiv = new Tree[Identifier](anchor.identifier) ⇢ scheme(selected.toList)
//          adding the equives (?????)
        val t = (RevisionDiff(List(), List(), List(equiv)), Rules.empty)
        new ActionSearchState(null, state.rewriteRules)
    }
  }
}

object LocateAction {
  private val placeholderEx = HyperTermIdentifier(Markers.placeholderEx.leaf)
}