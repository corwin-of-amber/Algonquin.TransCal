package synthesis.actions.operators

import relentless.rewriting.{RevisionDiff, Rules}
import relentless.rewriting.RuleBasedTactic.{Markers, ⇢}
import syntax.AstSugar.{TI, Term}
import syntax.Scheme
import synthesis.{HyperTerm, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState

/**
  * @author tomer
  * @since 11/18/18
  */
class ElaborateAction(goalScheme: Scheme) extends Action {
  override def apply(state: ActionSearchState): ActionSearchState = {

    val work = state.programs

    work.hyperGraph.findEdges(HyperTermIdentifier(Markers.goal.leaf)).headOption match {
      case Some(m) =>
        val elaborated = goalScheme(pickFirst(m.edgeType, work).subtrees)
        val original = work.reconstruct(m(1)).toStream.headOption getOrElse TI("?")
        val t = (RevisionDiff(List(), List(), List(original ⇢ elaborated)), Rules.empty)
        new ActionSearchState(null, state.rewriteRules)
      case None => state
    }
  }

  def pickFirst(match_ : HyperTerm, trie: Programs): Term = {
    trie.reconstruct(match_).next()
  }
}

