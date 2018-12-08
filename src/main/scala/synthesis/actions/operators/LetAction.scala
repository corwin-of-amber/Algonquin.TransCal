package synthesis.actions.operators

import relentless.rewriting._
import semantics.LambdaCalculus
import syntax.Scheme
import synthesis.actions.ActionSearchState
import LambdaCalculus.↦⁺
import RuleBasedTactic.⇢
import syntax.AstSugar._

/**
  * @author tomer
  * @since 11/18/18
  */
class LetAction(equalities: List[Scheme.Template], incorporate: Boolean = false) extends Action {

  val declaredVars: List[Term] = equalities flatMap (_.vars) map (T(_))
  def rules: List[RewriteRule] = {
    val rewrites: List[RewriteRule] = (equalities ++ derivedFrom(equalities)) map skolemize flatMap (RewriteRule(_, RewriteRule.Category.Definition))
    rewrites
  }

  val elab: List[Revision.Equivalence] = equalities map (_.template) collect { case T(`=`, List(lhs, rhs)) => lhs ⇢ rhs }

  object ST { def unapply(s: Scheme.Template) = Some((s.vars, s.template)) }

  def derivedFrom(equalities: Iterable[Scheme.Template]): Iterable[Scheme.Template] = equalities flatMap {
    case ST(vars, T(`=`, List(lhs, rhs@T(/, _)))) =>
      derivedFrom( rhs split / map (lhs =:= _) map (new Scheme.Template(vars, _)) )
    //rhs split / collect { case T(↦, List(pat, expr)) => (lhs :@ pat) =:= expr } map (new Scheme.Template(vars, _))
    case ST(vars, T(`=`, List(lhs, ↦⁺(va, body)))) => List(new Scheme.Template(vars, (lhs :@ va) =:= body))
    case _ => List()
  }

  def skolemize(equality: Scheme.Template): Scheme.Template = equality match {
    case ST(vars, t@T(`=`, List(lhs, _))) => new Scheme.Template(vars filter lhs.terminals.contains, t)
    case st => st
  }


  override def apply(state: ActionSearchState): ActionSearchState = {
    val t = (RevisionDiff(if (incorporate) elab map (_.rhs) else List(), declaredVars, List()), rules)
    new ActionSearchState(null, null)
  }
}

