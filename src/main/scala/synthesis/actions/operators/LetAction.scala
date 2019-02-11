package synthesis.actions.operators

import language.Language
import structures._
import syntax.AstSugar._
import syntax.{Identifier, Tree}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LetAction.LetMetadata
import synthesis.rewrites.RewriteRule
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, RepetitionTerm}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

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

  assert((Language.builtinDefinitions + Language.trueCondBuilderLiteral) contains term.root.literal.toString)

  // Start by naming lambdas and removing the bodies into rewrites.
  // I can give temporary name and later override them by using merge nodes
  private def createRewrites(t: Term): (Set[RewriteRule], Term) = {
    t.root match {
      case Language.lambdaId =>
        val newFunc = LetAction.functionNamer()
        val (innerRewrites, newTerm) = createRewrites(t.subtrees(1))

        val params = if (t.subtrees(0).root == Language.tupleId) t.subtrees(0).subtrees else List(t.subtrees(0))
        val condTerm = new Tree(newFunc, params)
        val (pattern, destination) = {
          val patterns = Programs.destructPatterns(condTerm, newTerm)
          (patterns(0), patterns(1))
        }

        val conditions: HyperPattern = {
          val rootEdge = pattern.findEdges(new ExplicitTerm(HyperTermIdentifier(newFunc))).head
          val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Ignored[HyperTermId, Int]()).get)
          pattern.addEdge(newRootEdge).removeEdge(rootEdge)
        }

        (innerRewrites + new RewriteRule(conditions, destination, metadataCreator(newFunc)), new Tree(newFunc))
      case Language.letId | Language.directedLetId =>
        val results = t.subtrees map (s => createRewrites(s))
        val (condition, destination) = {
          val temp = Programs.destructPatterns(results(0)._2, results(1)._2)
          (temp(0), temp(1))
        }
        val newRules: Set[RewriteRule] = {
          val optionalRule: Set[RewriteRule] =
            if (t.root == Language.directedLetId) Set.empty
            else Set(new RewriteRule(destination, condition, metadataCreator(t.subtrees(1).root)))
          optionalRule + new RewriteRule(condition, destination, metadataCreator(t.subtrees.head.root))
        }
        (newRules ++ results.flatMap(_._1), t)
      case _ =>
        val results = t.subtrees map (s => createRewrites(s))
        val subtrees = results.map(_._2)
        val innerRewrites = results.flatMap(_._1)
        (innerRewrites.toSet, new Tree(t.root, subtrees))
    }
  }

  protected val (rewrites, updatedTerm) = createRewrites(term)
  val rules: Set[RewriteRule] = rewrites

  def metadataCreator(funcName: Identifier): (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata = {
    (m1: Map[Int, HyperTermId], m2: Map[Int, HyperTermIdentifier]) => LetMetadata(funcName)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    // Take main expression and create a rewrite
    ActionSearchState(state.programs, state.rewriteRules ++ rewrites)
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