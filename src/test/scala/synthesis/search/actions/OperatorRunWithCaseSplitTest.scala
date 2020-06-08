package synthesis.search.actions

import org.scalatest.{FunSuite, Matchers}
import synthesis._
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.{AssociativeRewriteRulesDB, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{Identifier, TranscalParser}

class OperatorRunWithCaseSplitTest extends FunSuite with Matchers {
  val parser = new TranscalParser

  test("testApply") {
    val state1 = new DefAction(parser apply "whatever ?x = (x match ((true => 5) / (false => 5)))")(ActionSearchState(Programs.empty,
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    val state = state1.copy(programs = state1.programs.addTerm(parser parseExpression "a1 ||| whatever z"))
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser.parseExpression("5"))).head

    val res = new OperatorRunAction(4, Some(OperatorRunAction.GenerateGoalPredicate(HyperTermIdentifier(Identifier("a1")), pattern, root)))(state)
    val splitableState = ActionSearchState(
      state.programs.addTerm(parser parseExpression s"${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}(z, true, false)"),
      state.rewriteRules
    )
    res.programs.reconstruct(res.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(Identifier("a1"))).head.target) exists (_.root.literal == "5") shouldEqual false
    val res2 = new OperatorRunWithCaseSplit(4, Some(OperatorRunAction.GenerateGoalPredicate(HyperTermIdentifier(Identifier("a1")), pattern, root)))(splitableState)
    state should not equal res2
    res2.programs.reconstruct(res2.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(Identifier("a1"))).head.target) exists  (_.root.literal == "5") shouldEqual true
  }

}
