package synthesis.actions.operators.SPBE

import org.scalatest.{FunSuite, Matchers}
import synthesis.Programs
import synthesis.rewrites.RewriteSearchState
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class SyGuSRewriteRulesTest extends FunSuite with Matchers {
  private val parser = new TranscalParser()
  private val symbols: Set[AnnotatedTree] = Set("var1 : int",
    "var2 : string",
    "var3 : (list int)",
    "f1 : (string :> (list real))",
    "f2 : ((list int) :> int)")
    .map(parser.parseExpression)

  test("test creating rewrite rule per symbol") {
    new SyGuSRewriteRules(symbols).rewriteRules.size shouldBe symbols.size
  }

  test("test rewrite function correctly") {
    val g = Programs(parser("f >> Expression : list<real> [++]")).hyperGraph
    val ex = new SyGuSRewriteRules(symbols).rewriteRules.exists(r => r(new RewriteSearchState(g)).graph.edgeTypes.map(_.identifier).contains(Identifier("f1")))
    ex should be (true)
  }

  test("test rewrite variable and const correctly") {
    val g = Programs(parser("f >> Expression : string [++]")).hyperGraph
    val ex = new SyGuSRewriteRules(symbols).rewriteRules.exists(r => r(new RewriteSearchState(g)).graph.edgeTypes.map(_.identifier).contains(Identifier("var2")))
    ex should be (true)
  }

  test("test rewrite tuple correctly") {
    // var1 f2 var2
    val g = Programs(parser("_ -> Expression: (int, int, string)").subtrees(1)).hyperGraph
    val rules = new SyGuSRewriteRules(symbols).rewriteRules
    val newG = rules.foldLeft(new RewriteSearchState(g))((s, r) => r(s)).graph
    val wantedPattern = Programs.destructPattern(parser.parseExpression("(var1, f2(?x), var2)"))
    newG.findSubgraph[Int](wantedPattern).nonEmpty should be (true)
  }

  test("test RewriteRules for zero is a single rewrite rule for int") {
    val sygus = SyGuSRewriteRules(Set("_ -> 0").map(x => parser.apply(x).subtrees(1)))
    val rules = sygus.rewriteRules
    rules.size should be (1)
    val state = new RewriteSearchState(Programs.destruct(sygus.getExpressionAsTypedTree(Language.typeInt)))
    rules.head.apply(state).graph.size should be > state.graph.size
  }

}
