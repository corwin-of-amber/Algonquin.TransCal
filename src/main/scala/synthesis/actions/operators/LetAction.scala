package synthesis.actions.operators

import semantics.LambdaCalculus
import syntax.{Identifier, Scheme, Tree}
import synthesis.actions.ActionSearchState
import LambdaCalculus.↦⁺
import language.Language
import structures._
import syntax.AstSugar._
import synthesis.actions.operators.LetAction.LetMetadata
import synthesis.rewrites.{RewriteRule, Template}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}

/** Let action adds a rewrite rule to show the equality between the two templates.
  * This action also adds all equalities
  * This action does not add the templates to the graph. To also add the templates to the graph see DefAction.
  *
  * @author tomer
  * @since 11/18/18
  */
class LetAction(term: Term) extends Action {
  // TODO: check what skolemize was
  // TODO: Take care of splitting by adding inner lets

  // Beta reduction is done by adding rewrite rules and using flatten

  assert(Language.builtinDefinitions contains term.root.literal.toString)

  // Start by naming lambdas and removing the bodies into rewrites.
  // I can give temporary name and later override them by using merge nodes
  private def createRewrites(t: Term, env: Set[Set[Term]]): (Set[RewriteRule], Term) = {
    def getNewHoles(t: Term) = {
      val definitions = t.nodes.filter(n => n.root.literal.toString.startsWith("?"))
      val partners = definitions.map(i => new Tree(new Identifier(i.root.literal.toString.drop(1), i.root.kind, i.root.ns)))
      definitions zip partners map (t => Set(t._1, t._2)) toSet
    }

    t.root match {
      case Language.lambdaId =>
        val newFunc = LetAction.functionNamer()
        val newEnv = env ++ getNewHoles(t.subtrees.head)
        val (innerRewrites, newTerm) = createRewrites(t.subtrees(1), newEnv)

        val params = if (t.subtrees(0).root == Language.tupleId) t.subtrees(0).subtrees else List(t.subtrees(0))
        val condTerm = new Tree(newFunc, params)
        val pattern = Programs.destructPattern(condTerm, newEnv)
        val conditions: HyperPattern = {
          val rootEdge = pattern.findEdges(new ExplicitTerm(HyperTermIdentifier(newFunc))).head
          val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Ignored[HyperTermId, Int]()).get)
          pattern.addEdge(newRootEdge).removeEdge(rootEdge)
        }

        val destination = Programs.destructPattern(newTerm, newEnv)

        (innerRewrites + new RewriteRule(conditions, destination, metadataCreator(newFunc)), new Tree(newFunc))
      case Language.letId | Language.directedLetId =>
        val newEnv = env ++ getNewHoles(t.subtrees.head)
        val results = t.subtrees map (s => createRewrites(s, newEnv))
        val condition = Programs.destructPattern(results(0)._2, newEnv)
        val destination = Programs.destructPattern(results(1)._2, newEnv)
        val newRules: Set[RewriteRule] = {
          val optionalRule: Set[RewriteRule] =
            if (t.root == Language.directedLetId) Set.empty
            else Set(new RewriteRule(destination, condition, metadataCreator(t.subtrees(1).root)))
          optionalRule + new RewriteRule(condition, destination, metadataCreator(t.subtrees.head.root))
        }
        (newRules ++ results.flatMap(_._1), t)
        // TODO: split is 'or' or an 'and'
//      case Language.splitId =>
      case _ =>
        val results = t.subtrees map (s => createRewrites(s, env))
        val subtrees = results.map(_._2)
        val innerRewrites = results.flatMap(_._1)
        (innerRewrites.toSet, new Tree(t.root, subtrees))
    }
  }

  protected val (rewrites, updatedTerm) = createRewrites(term, Set.empty)

  def metadataCreator(funcName: Identifier): (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata = {
    (m1: Map[Int, HyperTermId], m2: Map[Int, HyperTermIdentifier]) => LetMetadata(funcName)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    // Take main expression and create a rewrite
    new ActionSearchState(state.programs, state.rewriteRules ++ rewrites)
  }
}

object LetAction {
  protected val functionNamer: () => Identifier = {
    val creator = Stream.from(language.Language.arity.size).iterator
    () => I(s"f${creator.next()}")
  }

  case class LetMetadata(funcName: Identifier) extends Metadata {
    override val toStr = s"LetMetadata($funcName)"
  }

}