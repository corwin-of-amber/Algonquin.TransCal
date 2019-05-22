package synthesis.actions.operators.SPBE

import org.scalatest.{FunSuite, Matchers}
import synthesis.Programs
import synthesis.rewrites.RewriteSearchState
import transcallang.{Language, TranscalParser}

class SyGuSRewriteRulesTest extends FunSuite with Matchers {
  val parser = new TranscalParser()

  test("test RewriteRules for zero is a single rewrite rule for int") {
    val sygus = SyGuSRewriteRules(Set("_ -> 0").map(x => parser.apply(x).subtrees(1)))
    val rules = sygus.rewriteRules
    rules.size should be (1)
    val state = new RewriteSearchState(Programs.destruct(sygus.getExpressionAsTypedTree(Language.typeInt)))
    rules.head.apply(state).graph.size should be > state.graph.size
  }

}
