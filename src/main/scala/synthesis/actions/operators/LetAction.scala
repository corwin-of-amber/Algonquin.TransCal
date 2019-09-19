package synthesis.actions.operators

import structures._
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, RepetitionTerm}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

/** Let action adds a rewrite rule to show the equality between the two templates.
  * This action also adds all equalities
  * This action does not add the templates to the graph. To also add the templates to the graph see DefAction.
  *
  * @author tomer
  * @since 11/18/18
  */
class LetAction(val typedTerm: AnnotatedTree, val allowExistential: Boolean = true, cleanTypes: Boolean = true) extends Action {
  // TODO: check what skolemize was
  // Beta reduction is done by adding rewrite rules and using flatten
  private val term = if (cleanTypes) typedTerm.map(i => i.copy(annotation = None)) else typedTerm

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
      val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Ignored[HyperTermId, Int]()).get)
      pattern.+(newRootEdge).-(rootEdge)
    }

    // TODO: Add non existantial double directed rewrites for matches
    val newRewrite = new RewriteRule(premise, conclusion, metadataCreator(funcName), Programs.termToString(term))
    if (newRewrite.isExistential) logger.info(s"Created Existential rule: ${Programs.termToString(condTerm)} >> ${Programs.termToString(newTerm)}")
    (innerRewrites + newRewrite, AnnotatedTree.identifierOnly(funcName))
  }

  // Start by naming lambdas and removing the bodies into rewrites.
  // I can give temporary name and later override them by using merge nodes
  private def createRewrites(t: AnnotatedTree, optName: Option[Identifier] = None): (Set[RewriteRule], AnnotatedTree) = {
    t.root match {
      case i: Identifier if Language.builtinDefinitions.contains(i) =>
        val conclusionIsSingle = t.subtrees.last.size == 1 && (t.subtrees.last.root.literal.startsWith("?") || t.nodes.map(_.root.literal).filter(_.startsWith("?")).map(_.drop(1)).contains(t.subtrees.last.root.literal))
        val premiseIsSingle = t.subtrees.last.size == 1 && (t.subtrees.head.root.literal.startsWith("?") || t.nodes.map(_.root.literal).filter(_.startsWith("?")).map(_.drop(1)).contains(t.subtrees.head.root.literal))

        val results = t.subtrees map (s => createRewrites(s, Some(t.subtrees(0).root)))
        val (premise, conclusion) = {
          val temp = Programs.destructPatterns(Seq(results(0)._2, results(1)._2),
            mergeRoots = !Language.builtinLimitedDefinitions.contains(i))
          (temp.head, temp.last)
        }

        val newRules: Set[RewriteRule] = {
          val optionalRule: Set[RewriteRule] =
            if (Language.builtinDirectedDefinitions.contains(t.root)) Set.empty
            else {
              val toUsePremise = if (!premiseIsSingle) premise
                                else Programs.destructPatterns(Seq(AnnotatedTree.withoutAnnotations(Language.idId, Seq(results(0)._2)), results(1)._2), mergeRoots = !Language.builtinLimitedDefinitions.contains(i)).head
              Set(new RewriteRule(conclusion, toUsePremise, metadataCreator(t.subtrees(1).root), Programs.termToString(t)))
            }
          val toUseConclusion = if (!conclusionIsSingle) conclusion
                                else Programs.destructPatterns(Seq(results(0)._2, AnnotatedTree.withoutAnnotations(Language.idId, Seq(results(1)._2))), mergeRoots = !Language.builtinLimitedDefinitions.contains(i)).last
          optionalRule
          val requiredRule = Set(new RewriteRule(premise, toUseConclusion, metadataCreator(t.subtrees.head.root), Programs.termToString(t)))
          Set(optionalRule, requiredRule).filter(allowExistential || _.forall(!_.isExistential)).flatten
        }
        if (newRules.exists(_.isExistential)) logger.info(s"Created Existential rule ${Programs.termToString(results.head._2)} ${t.root} ${Programs.termToString(results(1)._2)}")

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
    (_: Map[Int, HyperTermId], _: Map[Int, HyperTermIdentifier]) => LetAction.LetMetadata(funcName)
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