package synthesis.rewrites

import org.scalatest.{FunSuite, Matchers}
import structures.mutable.CompactHyperGraph
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{DefAction, RecursiveTimeComplexActionTest}
import synthesis.{Programs, TimeComplexRewriteRulesDB}
import transcallang.{Language, TranscalParser}

class MatchTimeComplexRewriteTest extends FunSuite with Matchers {
  test("single match test") {
    val parser = new TranscalParser

    val hyperGraphBasic = {
      Seq(
        new DefAction(parser.apply("example ?i = i match ((0 => true) / (0 => true))")),
        new DefAction(parser.apply("timecomplex i 0 = timecomplexTrue")),
      ).foldLeft(ActionSearchState(Programs.empty, Set.empty))((s, action) => action apply s).programs.hyperGraph
    }
    val basicSize = hyperGraphBasic.size
    val hyperGraphStart = CompactHyperGraph.empty ++ RecursiveTimeComplexActionTest.populate(TimeComplexRewriteRulesDB.rewriteRules - MatchTimeComplexRewrite, hyperGraphBasic)
    val startSize = hyperGraphStart.size
    assume(startSize > basicSize)

    val hyperGraphResult = MatchTimeComplexRewrite(RewriteSearchState(hyperGraphStart)).graph
    val resultSize = hyperGraphResult.size
    resultSize should be > startSize

    val wantedMatch = hyperGraphResult.find(_.edgeType.identifier == Language.matchId)
    wantedMatch should not be empty
    val a = Programs(hyperGraphResult).reconstructWithTimeComplex(wantedMatch.get.target)
    a should not be empty
  }
}
