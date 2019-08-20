package synthesis.rewrites

import org.scalatest.{FunSuite, Matchers}
import structures.mutable.VersionedHyperGraph
import synthesis.{Programs, SpaceComplexRewriteRulesDB, TimeComplexRewriteRulesDB}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{DefAction, RecursiveTimeComplexActionTest}
import transcallang.TranscalParser

class MatchTimeComplexRewriteTest extends FunSuite with Matchers {
  test("single match test") {
    val parser = new TranscalParser

    val hyperGraphBasic = {
      Seq(
        new DefAction(parser.apply("example ?i = i match ((0 => true) / (0 => true))")),
        new DefAction(parser.apply("timecomplex 0 0 = timecomplexTrue")),
        new DefAction(parser.apply("timecomplex i 0 = timecomplexTrue")),
        new DefAction(parser.apply("timecomplex true 0 = timecomplexTrue")),
      ).foldLeft(ActionSearchState(Programs.empty, Set.empty))((s, action) => action apply s).programs.hyperGraph
    }
    val basicSize = hyperGraphBasic.size
    val hyperGraphStart = VersionedHyperGraph.empty ++ RecursiveTimeComplexActionTest.populate(SpaceComplexRewriteRulesDB.rewriteRules ++ TimeComplexRewriteRulesDB.rewriteRules, hyperGraphBasic)
    val startSize = hyperGraphStart.size
    assume(startSize > basicSize)

    val hyperGraphResult = MatchTimeComplexRewrite(RewriteSearchState(hyperGraphStart)).graph
    val resultSize = hyperGraphResult.size
    resultSize should be > startSize

    val wantedMatch = hyperGraphResult.find(_.edgeType.identifier.literal == "match")
    wantedMatch should not be empty
    val a = Programs(hyperGraphResult).reconstructWithTimeComplex(wantedMatch.get.target)
    a should not be empty
  }
}
