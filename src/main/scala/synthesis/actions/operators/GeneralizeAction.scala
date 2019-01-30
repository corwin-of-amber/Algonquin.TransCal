package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import syntax.AstSugar._
import relentless.rewriting._
import synthesis.actions.operators.GeneralizeAction.{DerivedDefinition, NUM_ALTS_TO_SHOW, generalize}
import relentless.rewriting.RuleBasedTactic.⇢
import syntax.AstSugar.Term
import syntax.{Identifier, Scheme, Strip, Tree}
import synthesis.HyperTermIdentifier
import synthesis.actions.ActionSearchState

/**
  * @author tomer
  * @since 11/18/18
  */
class GeneralizeAction(anchor: HyperTermIdentifier, leaves: List[Term], name: Term) extends Action with LazyLogging {
  override def apply(state: ActionSearchState): ActionSearchState = {

    val roots = state.programs.hyperGraph.findEdges(anchor) map (_.target)

    val context: Set[Term] = Set.empty  // s.env.vars.toSet

    // Reconstruct and generalize
    val gen =
      for (root <- roots) yield {
        (for (term <- state.programs.reconstruct(root);
              genTerm <- generalize(term, leaves, context)) yield {
          val vas = leaves.indices map Strip.greek map (TV(_))

          val equivs = Set(term ⇢ (name :@ leaves), name ⇢ (vas ↦: genTerm))
          val ruleDefs = Set((name :@ vas) =:= genTerm)
          DerivedDefinition(vas, equivs, ruleDefs)
        }).take(NUM_ALTS_TO_SHOW)
      }

    /* select just the first generalization */
    gen.flatten.headOption match {
      case None => state
      case Some(derivedDef) =>
        for (e <- derivedDef.equivs)
          logger.debug(e.toString)
        val varIds = derivedDef.vars.toList map (_.leaf)
//        val rules = new Let((derivedDef.ruleDefs map (new Scheme.Template(varIds, _))).toList).rules
        val rules = derivedDef.ruleDefs flatMap (r => new LetAction(r).rules)
        state.copy(rewriteRules = state.rewriteRules ++ rules)
    }
  }
}

object GeneralizeAction {

  final val NUM_ALTS_TO_SHOW = 10

  def generalize(t: Term, leaves: List[Term], context: Set[Term]): Option[Term] = {
    //println(s"[generalize] ${t.toPretty}  with context  ${context}")

    leaves.indexOf(t) match {
      case -1 => if (context contains t) None else T_?(t.root)(t.subtrees map (generalize(_, leaves, context)))
      case idx => Some( TI(Strip.greek(idx)) )
    }
  }

  /** Construct Some[Term] only if all subtrees are defined. Otherwise, None. */
  def T_?(root: Identifier)(subtrees: List[Option[Term]]): Option[Tree[Identifier]] =
    if (subtrees contains None) None else Some(T(root)(subtrees map (_.get)))

  case class DerivedDefinition(vars: Seq[Term], equivs: Set[Revision.Equivalence], ruleDefs: Set[Term])
}
