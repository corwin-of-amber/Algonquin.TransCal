package synthesis.actions.operators

import transcallang.{AnnotatedTree, Identifier, Language}
import structures._
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
class LetAction(val term: AnnotatedTree) extends Action {
  // TODO: check what skolemize was
  // Beta reduction is done by adding rewrite rules and using flatten

  assert((Language.builtinDefinitions :+ Language.trueCondBuilderId :+ Language.andCondBuilderId :+ Language.limitedAndCondBuilderId) contains term.root)

  private def createRuleWithNameFromLambda(args: AnnotatedTree, body: AnnotatedTree, funcName: Identifier): (Set[RewriteRule], AnnotatedTree) = {
    val (innerRewrites, newTerm) = createRewrites(body)

    val params = if (args.root == Language.tupleId) args.subtrees else List(args)
    val condTerm = AnnotatedTree(funcName, params, Seq.empty)
    // TODO: Add suffix to conclusion (currently it is lost because of ignore)
    val (pattern, conclusion) = {
      val patterns = Programs.destructPatterns(Seq(condTerm, newTerm))
      (patterns(0), patterns(1))
    }

    val premise: HyperPattern = {
      val rootEdge = pattern.findEdges(new ExplicitTerm(HyperTermIdentifier(funcName))).head
      val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Ignored[HyperTermId, Int]()))
      pattern.+(newRootEdge).-(rootEdge)
    }

    // TODO: Add non existantial double directed rewrites for matches
    (innerRewrites + new RewriteRule(premise, conclusion, metadataCreator(funcName)), AnnotatedTree.identifierOnly(funcName))
  }

  // Start by naming lambdas and removing the bodies into rewrites.
  // I can give temporary name and later override them by using merge nodes
  private def createRewrites(t: AnnotatedTree, optName: Option[Identifier] = None): (Set[RewriteRule], AnnotatedTree) = {
    t.root match {
      case i: Identifier if Language.builtinDefinitions.contains(i) =>
        val results = t.subtrees map (s => createRewrites(s, Some(t.subtrees(0).root)))
        val (premise, conclusion) = {
          val temp = Programs.destructPatterns(Seq(results(0)._2, results(1)._2),
            mergeRoots = !Language.builtinLimitedDefinitions.contains(i))
          (temp(0), temp(1))
        }

        val newRules: Set[RewriteRule] = {
          val optionalRule: Set[RewriteRule] =
            if (Language.builtinDirectedDefinitions.contains(t.root)) Set.empty
            else Set(new RewriteRule(conclusion, premise, metadataCreator(t.subtrees(1).root)))
          optionalRule + new RewriteRule(premise, conclusion, metadataCreator(t.subtrees.head.root))
        }

        if (premise == conclusion) (results.flatMap(_._1).toSet, t.copy(subtrees = results.map(_._2)))
        else (newRules ++ results.flatMap(_._1), t.copy(subtrees = results.map(_._2)))
      case Language.lambdaId =>
        val newFunc = optName.getOrElse(LetAction.functionNamer(t))
        createRuleWithNameFromLambda(t.subtrees(0), t.subtrees(1), newFunc)
      case Language.matchId =>
        val param = t.subtrees.head
        val newFunc = optName.getOrElse(LetAction.functionNamer(t))
        val guarded = t.subtrees.tail
        val innerRules = guarded.flatMap(g => createRuleWithNameFromLambda(g.subtrees(0), g.subtrees(1), newFunc)._1).toSet
        (innerRules, AnnotatedTree(newFunc, if (param.root == Language.tupleId) param.subtrees else List(param), Seq.empty))
      case _ =>
        val results = t.subtrees map (s => createRewrites(s))
        val subtrees = results.map(_._2)
        val innerRewrites = results.flatMap(_._1)
        (innerRewrites.toSet, AnnotatedTree(t.root, subtrees, Seq.empty))
    }
  }

  protected val (rewrites, updatedTerm) = createRewrites(term)
  val rules: Set[RewriteRule] = rewrites

  def metadataCreator(funcName: Identifier): (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata = {
    (_: Map[Int, HyperTermId], _: Map[Int, HyperTermIdentifier]) => LetMetadata(funcName)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    // Take main expression and create a rewrite
    ActionSearchState(state.programs, state.rewriteRules ++ rewrites)
  }
}

object LetAction {
  private val creator = Stream.from(transcallang.Language.arity.size).iterator

  protected def functionNamer(term: AnnotatedTree): Identifier = {
    Identifier(s"f${creator.next()}")
  }

  case class LetMetadata(funcName: Identifier) extends Metadata {
    override val toStr = s"LetMetadata($funcName)"
  }

}