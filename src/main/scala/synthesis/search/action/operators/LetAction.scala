package synthesis.search.action.operators

import structures._
import synthesis.search.rewrite.operators.RewriteRule
import synthesis.search.rewrite.operators.RewriteRule.HyperPattern
import synthesis.search.rewrite.operators.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm}
import synthesis.search.action.ActionSearchState
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
  // Beta reduction is done by adding rewrite rules and using flatten
  private val term = if (cleanTypes) typedTerm.cleanTypes else typedTerm

  assert((Language.builtinDefinitions :+ Language.trueCondBuilderId :+ Language.andCondBuilderId :+ Language.limitedAndCondBuilderId) contains term.root)

  private def createRuleWithNameFromLambda(args: AnnotatedTree, body: AnnotatedTree, funcName: Identifier, bidirectional: Boolean): (Set[RewriteRule], AnnotatedTree) = {
    val (innerRewrites, newTerm) = createRewrites(body)

    val params = if (args.root == Language.tupleId) args.subtrees else List(args)
    val condTerm = AnnotatedTree.withoutAnnotations(funcName, params)

    val (((lhs, lRoot), (rhsToUse, rtuRoot)), ((lhsToUse, ltuRoot), (rhs, rRoot))) = fixSingles(condTerm, newTerm, mergeRoots = true)
    val repetitionStart = {
      val ids = (lhs.nodes ++ rhsToUse.nodes ++ rhs.nodes ++ lhsToUse.nodes).collect({ case ReferenceTerm(i) => i })
      if (ids.isEmpty) 50
      else ids.max + 50
    }
    val repetitionTerm = RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Stream.from(repetitionStart).map(ReferenceTerm(_))).get

    val rootEdge = lhs.findEdges(new ExplicitTerm(HyperTermIdentifier(funcName))).head
    val premise: HyperPattern = {
      val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ repetitionTerm)
      lhs.+(newRootEdge).-(rootEdge)
    }

    // There is no promise that the conclusion has a single edge from target.
    // Can either apply params on top or insert them inside the different edges.
    // If there is a chance that a rule will not apply because of pre-flatten then leave it outside.
    val conclusion = {
      rhsToUse.+(HyperEdge(
        ReferenceTerm(repetitionStart - 2),
        ExplicitTerm(HyperTermIdentifier(Language.applyId)),
        // rootEdge target is equal to conclusion root because of merge roots
        Seq(rtuRoot, repetitionTerm),
        EmptyMetadata))
    }

    val preProcessor = (idMap: Map[Int, HyperTermId], edgeMap: Map[Int, HyperTermIdentifier], graph: RewriteRule.MutableHyperPattern) => {
      if (idMap.contains(repetitionStart))
        graph
      else
        graph --= graph.findByTarget(ReferenceTerm(repetitionStart - 2))
    }

    // TODO: Add non existantial double directed rewrites for matches
    val newRewrite = new RewriteRule(premise, conclusion, metadataCreator(funcName), Programs.termToString(term), Seq(preProcessor))
    val optionalRewrite = (if(body.root != Language.limitedAndCondBuilderId && body.root != Language.andCondBuilderId) {
        val newPremise = {
          assert(rhs.findByTarget(rRoot).size == 1)
          val rootEdge = rhs.findByTarget(rRoot).head
          val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ repetitionTerm)
          rhs.-(rootEdge).+(newRootEdge)
        }
        val newConclusion = {
          lhsToUse.+(HyperEdge(
            ReferenceTerm(repetitionStart - 2),
            ExplicitTerm(HyperTermIdentifier(Language.applyId)),
            // rootEdge target is equal to conclusion root because of merge roots
            Seq(ltuRoot, repetitionTerm),
            EmptyMetadata))
        }
        Some(new RewriteRule(newPremise, newConclusion, metadataCreator(funcName), Programs.termToString(term), Seq(preProcessor)))
      }
      else {
        logger.warn("Can't add bidirectional rewrite with varargs due to and condition builder, Adding without varargs.")
        Some(new RewriteRule(rhs, lhsToUse, metadataCreator(funcName), Programs.termToString(term), Seq(preProcessor)))
      }).filter(bidirectional && !_.isExistential)
    if (newRewrite.isExistential)
      logger.info(s"Created Existential rule: ${Programs.termToString(condTerm)} >> ${Programs.termToString(newTerm)}")
    (innerRewrites + newRewrite ++ optionalRewrite, AnnotatedTree.identifierOnly(funcName))
  }

  private def fixSingles(lhs: AnnotatedTree, rhs: AnnotatedTree, mergeRoots: Boolean) = {
    def checkSingle(t1: AnnotatedTree, t2: AnnotatedTree) = {
      t1.size == 1 && (t1.root.literal.startsWith("?") ||
        t2.nodes.map(_.root.literal).filter(_.startsWith("?")).map(_.drop(1)).contains(t1.root.literal))
    }

    val lhsToUse =
      if (checkSingle(lhs, rhs)) AnnotatedTree.withoutAnnotations(Language.idId, Seq(lhs))
      else lhs
    val rhsToUse =
      if (checkSingle(rhs, lhs)) AnnotatedTree.withoutAnnotations(Language.idId, Seq(rhs))
      else rhs
    val rhsPatterns = Programs.destructPatternsWithRoots(Seq(lhs, rhsToUse), mergeRoots = mergeRoots)
    val lhsPatterns = Programs.destructPatternsWithRoots(Seq(lhsToUse, rhs), mergeRoots = mergeRoots)
    ((rhsPatterns(0), rhsPatterns(1)), (lhsPatterns(0), lhsPatterns(1)))
  }

  // Start by naming lambdas and removing the bodies into rewrites.
  // I can give temporary name and later override them by using merge nodes
  private def createRewrites(t: AnnotatedTree, optName: Option[Identifier] = None): (Set[RewriteRule], AnnotatedTree) = {
    t.root match {
      case i: Identifier if Language.builtinDefinitions.contains(i) =>
        val results = t.subtrees map (s => createRewrites(s, Some(t.subtrees.head.root.copy(literal = t.subtrees.head.root + "'"))))
        val (((lhs, _), (rhsToUse, _)), ((lhsToUse, _), (rhs, _))) = fixSingles(results.head._2, results.last._2, !Language.builtinLimitedDefinitions.contains(i))

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
          val requiredRule = Set(new RewriteRule(premise, toUseConclusion, metadataCreator(t.subtrees.head.root), Programs.termToString(t)))
          Set(optionalRule, requiredRule).filter(allowExistential || _.forall(!_.isExistential)).flatten
        }
        if (newRules.exists(_.isExistential)) logger.info(s"Created Existential rule ${Programs.termToString(results.head._2)} ${t.root} ${Programs.termToString(results(1)._2)}")

        if (rhsToUse == lhsToUse) (results.flatMap(_._1).toSet, t.copy(subtrees = results.map(_._2)))
        else (newRules ++ results.flatMap(_._1), t.copy(subtrees = results.map(_._2)))
      case Language.lambdaId =>
        val newFunc = optName.map(x => x.copy(literal = x.literal + "'")).getOrElse(LetAction.functionNamer(t))
        createRuleWithNameFromLambda(t.subtrees(0), t.subtrees(1), newFunc, true)
      case Language.matchId =>
        val param = t.subtrees.head
        val newFunc = optName.map(x => x.copy(literal = x.literal + "'")).getOrElse(LetAction.functionNamer(t))
        val guarded = t.subtrees.tail
        val innerRules = guarded.flatMap(g => createRuleWithNameFromLambda(g.subtrees(0), g.subtrees(1), newFunc, true)._1).toSet
        (innerRules, AnnotatedTree(newFunc, if (param.root == Language.tupleId) param.subtrees else List(param), Seq.empty))
      case _ =>
        val results = t.subtrees map (s => createRewrites(s, optName))
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