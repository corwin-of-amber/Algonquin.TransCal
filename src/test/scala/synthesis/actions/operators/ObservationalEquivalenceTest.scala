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

  test("testGetEquives on filtermap And mapfilter applications") {
    val mapRules = new LetAction(parser("map ?f ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => f x :: map f xs))")).rules
    val filterRules = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => ( (p x) match (true => x :: (filter p xs)) / false => filter p xs) ))")).rules
    val predRules = new LetAction(parser("pred ?t = t match ((x => true) / (y => false))")).rules
    val pred2Rules = new LetAction(parser("pred2 ?t = t match ((id x => true) / (id y => false))")).rules
    val mapperRules = new LetAction(parser("mapper ?t = id t")).rules
    val tempTerm = parser("filter(pred2, map(mapper, x :: y :: nil)) = map(mapper, filter(pred, x :: y :: nil))")
    val equivs = ObservationalEquivalence.getEquives(predRules ++ pred2Rules ++ mapperRules ++ mapRules ++ filterRules ++ SimpleRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules, Seq(tempTerm.subtrees(0), tempTerm.subtrees(1)))
    equivs.size shouldEqual (1)
    equivs.head.size shouldEqual (2)
  }

}
