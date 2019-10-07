package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.ActionSearchState
import synthesis._
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
    res2.exists(_.size > 1) shouldBe true
  }

}
