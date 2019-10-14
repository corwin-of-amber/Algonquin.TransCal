package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.ActionSearchState
import synthesis._
import synthesis.rewrites.RewriteSearchState
import transcallang.{Identifier, TranscalParser}

class CaseSplitActionTest extends FunSuite with Matchers {
  val parser = new TranscalParser

  test("test splitting to true and false finds it is the same") {
    val state1 = new DefAction(parser apply "whatever ?x = (x match ((true => 5) / (false => 5)))")(ActionSearchState(Programs.empty,
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    val state = state1.copy(programs = state1.programs.addTerm(parser parseExpression "whatever z"))
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser.parseExpression("5"))).head

    val res = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), pattern, root)(state)
    assert(state == res)
    val splitableState = ActionSearchState(
      state.programs.addTerm(parser parseExpression s"${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}(z, true, false)"),
      state.rewriteRules
    )
    val res2 = new CaseSplitAction(splitableState.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId)).head).getFoundConclusions(splitableState)
    val newState = ObservationalEquivalence.mergeConclusions(new RewriteSearchState(splitableState.programs.hyperGraph), res2.toSeq)
    newState.graph.findSubgraph[Int](Programs.destructPattern(parser parseExpression "whatever z ||| 5" map (_.copy(annotation = None)))).nonEmpty shouldBe true
  }

  test("test splitting filter p filter q") {
    var state = new DefAction(parser apply "filter q (filter p (x :: y :: ⟨⟩)) = filter q (filter p l)")(ActionSearchState(Programs.empty,
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    state = new DefAction(parser apply "filter p (filter q (x :: y :: ⟨⟩)) = filter p (filter q l)")(state)
    state = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(state)
    state = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(state)
    state = new OperatorRunAction(maxSearchDepth = 4)(state)
    val res = new CaseSplitAction(splitterChooser = None, splitDepthOption = Some(2), maxDepthOption = None)
      .getFoundConclusions(state)
    res.exists(_.size > 1) shouldBe true
  }
}
