//package synthesis.actions.operators
//
//import semantics.LambdaCalculus
//import syntax.Scheme
//import synthesis.actions.ActionSearchState
//import LambdaCalculus.↦⁺
//import structures.HyperEdge
//import syntax.AstSugar._
//import synthesis.rewrites.RewriteRule
//import synthesis.{HyperTermId, HyperTermIdentifier}
//import synthesis.rewrites.RewriteRule.HyperPattern
//import synthesis.rewrites.RewriteSearchState.HyperGraph
//import synthesis.rewrites.Template.TemplateTerm
//
///** Let action adds a rewrite rule to show the equality between the two templates.
//  * This action also adds all equalities
//  * This action does not add the templates to the graph. To also add the templates to the graph see DefAction.
//  *
//  * @author tomer
//  * @since 11/18/18
//  */
//class LetAction(lhs: HyperPattern, rhs: HyperPattern, biDirectional: Boolean) extends Action {
//  // TODO: check what skolemize was
//  // TODO: make all function definitions into lambdas so we will have correct rewriting
//
//  DO BETA REDUCTION
//
//  def rules: List[RewriteRule] = {
//    val rewrites: List[RewriteRule] = (equality ++ derivedFrom(equality)) map skolemize flatMap (RewriteRule(_, RewriteRule.Category.Definition))
//    rewrites
//  }
//
//  val elab: List[Revision.Equivalence] = equality map (_.template) collect { case T(`=`, List(lhs, rhs)) => lhs ⇢ rhs }
//
//
//  override def apply(state: ActionSearchState): ActionSearchState = {
//    val newRules = Set(new RewriteRule(lhs, rhs, RewriteRule.))
//    new ActionSearchState(state.programs, state.rewriteRules ++ newRules)
//  }
//}