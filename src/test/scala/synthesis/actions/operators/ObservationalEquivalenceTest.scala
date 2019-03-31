package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import synthesis.{AssociativeRewriteRulesDB, SimpleRewriteRulesDB}
import transcallang.TranscalParser

class ObservationalEquivalenceTest extends FunSuite with Matchers {
  val parser = new TranscalParser

  test("testGetEquives on add commutative") {
    val tempTerm = parser("1 + x = x + 1")
    val equivs = ObservationalEquivalence.getEquives(SimpleRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules, Seq(tempTerm.subtrees(0), tempTerm.subtrees(1)))
    equivs.size shouldEqual (1)
    equivs.head.size shouldEqual (2)
  }

}
