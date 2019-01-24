//package synthesis.actions.operators
//
//import semantics.LambdaCalculus
//import syntax.{Identifier, Scheme, Tree}
//import synthesis.actions.ActionSearchState
//import LambdaCalculus.↦⁺
//import language.Language
//import structures.HyperEdge
//import syntax.AstSugar._
//import synthesis.rewrites.{RewriteRule, Template}
//import synthesis.{HyperTermId, HyperTermIdentifier}
//import synthesis.rewrites.RewriteRule.HyperPattern
//import synthesis.rewrites.RewriteSearchState.HyperGraph
//import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
//
///** Let action adds a rewrite rule to show the equality between the two templates.
//  * This action also adds all equalities
//  * This action does not add the templates to the graph. To also add the templates to the graph see DefAction.
//  *
//  * @author tomer
//  * @since 11/18/18
//  */
//class LetAction(term: Term) extends Action {
//  // TODO: check what skolemize was
//
//  // Beta reduction is done by adding rewrite rules and using flatten
//
//  assert(Language.builtinDefinitions contains term.root.literal.toString)
//
//  // Start by naming lambdas and removing the bodies into rewrites.
//  // I can give temporary name and later override them by using merge nodes
//  private val updatedTerm: Term = {
//    def renameLambdas(t: Term): (Set[RewriteRule], Term) = {
//      val results = t.subtrees map renameLambdas
//      val subtrees = results.map(_._2)
//      val rewrites = results.flatMap(_._1)
//      t.root match {
//        case Language.lambdaId =>
//          val newFunc = LetAction.functionNamer()
//          val params = if(subtrees(0).root == Language.tupleId) subtrees(0).subtrees else List(subtrees(0))
//          val conditions = Template(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(newFunc)), params)
//          (Set(), new Tree(newFunc))
//        case _ => (rewrites.toSet, new Tree(t.root, subtrees))
//      }
//    }
//    renameLambdas(term)
//  }
//
//  override def apply(state: ActionSearchState): ActionSearchState = {
//
//    // Take main expression and create a rewrite
//    val newRules = Set(new RewriteRule(lhs, rhs, RewriteRule.))
//    new ActionSearchState(state.programs, state.rewriteRules ++ newRules)
//  }
//}
//
//object LetAction {
//  protected val functionNamer: () => Identifier = {
//    val creator = Stream.from(language.Language.arity.size).iterator
//    () => I(s"f${creator.next()}")
//  }
//}