package synthesis.actions.operators

import transcallang.Language
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
class LetAction(val term: Term) extends Action {
  // TODO: check what skolemize was
  // Beta reduction is done by adding rewrite rules and using flatten

  assert((Language.builtinDefinitions + Language.trueCondBuilderLiteral + Language.andCondBuilderId) contains term.root.literal.toString)

  private def createRuleWithName(args: Term, body: Term, funcName: Identifier): (Set[RewriteRule], Term) = {
    val (innerRewrites, newTerm) = createRewrites(body)

    val params = if (args.root == Language.tupleId) args.subtrees else List(args)
    val condTerm = new Tree(funcName, params)
    val (pattern, conclusion) = {
      val patterns = Programs.destructPatterns(Seq(condTerm, newTerm))
      (patterns(0), patterns(1))
    }

    val premise: HyperPattern = {
      val rootEdge = pattern.findEdges(new ExplicitTerm(HyperTermIdentifier(funcName))).head
      val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Ignored[HyperTermId, Int]()).get)
      pattern.addEdge(newRootEdge).removeEdge(rootEdge)
    }

    (innerRewrites + new RewriteRule(premise, conclusion, metadataCreator(funcName)), new Tree(funcName))
  }

  // Start by naming lambdas and removing the bodies into rewrites.
  // I can give temporary name and later override them by using merge nodes
  private def createRewrites(t: Term): (Set[RewriteRule], Term) = {
    t.root match {
      case Language.lambdaId =>
        val newFunc = LetAction.functionNamer()
        createRuleWithName(t.subtrees(0), t.subtrees(1), newFunc)
      case Language.letId | Language.directedLetId =>
        val results = t.subtrees map (s => createRewrites(s))
        val (premise, conclusion) = {
          val temp = Programs.destructPatterns(Seq(results(0)._2, results(1)._2))
          (temp(0), temp(1))
        }
        val newRules: Set[RewriteRule] = {
          val optionalRule: Set[RewriteRule] =
            if (t.root == Language.directedLetId) Set.empty
            else Set(new RewriteRule(conclusion, premise, metadataCreator(t.subtrees(1).root)))
          optionalRule + new RewriteRule(premise, conclusion, metadataCreator(t.subtrees.head.root))
        }
        (newRules ++ results.flatMap(_._1), t)
      case Language.matchId =>
        val param = t.subtrees.head
        val newFunc = LetAction.functionNamer()
        val guarded = t.subtrees.tail
        val innerRules = guarded.flatMap(g => createRuleWithName(g.subtrees(0), g.subtrees(1), newFunc)._1).toSet
        (innerRules, new Tree(newFunc, if (param.root == Language.tupleId) param.subtrees else List(param)))
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
    val creator = Stream.from(transcallang.Language.arity.size).iterator
    () => I(s"f${creator.next()}")
  }

  case class LetMetadata(funcName: Identifier) extends Metadata {
    override val toStr = s"LetMetadata($funcName)"
  }

}