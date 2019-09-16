package synthesis.actions

import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.operators.{CaseSplitAction, ElaborateAction}
import synthesis.{AssociativeRewriteRulesDB, HyperTermIdentifier, Programs, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{Identifier, TranscalParser}

class CaseSplitActionTest extends FunSuite with Matchers {
  val parser = new TranscalParser

  test("test splitting to true and false finds it is the same") {
    val state = ActionSearchState(Programs(parser parseExpression "a1 ||| (x match ((true => 5) / (false => 5)))"),
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules)
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser.parseExpression("5"))).head

    val res = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), pattern, root)(state)
    assert(state == res)
    val splitableState = ActionSearchState(
      state.programs.addTerm(parser parseExpression s"${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}(x, true, false)"),
      state.rewriteRules
    )
    new CaseSplitAction()(splitableState) should not be (splitableState)
  }

}
