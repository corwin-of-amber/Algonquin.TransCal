package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.Template.ReferenceTerm
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, Programs, SimpleRewriteRulesDB}
import transcallang.{AnnotatedTree, Language, TranscalParser}

class ObservationalEquivalenceTest extends FunSuite with Matchers {
  val parser = new TranscalParser
  test("testFromTerms") {
    val eqiuivalentTerms = Seq(
      "x + y",
      "y + x",
      s"x :: y :: ${Language.nilId.literal}",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal})",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal}) ++ ${Language.nilId.literal}"
    ).map(parser.parseExpression)
    val res = new ObservationalEquivalence(3).fromTerms(eqiuivalentTerms, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules)
    val expected = Set(eqiuivalentTerms.take(2).toSet, eqiuivalentTerms.drop(2).toSet)
    expected shouldEqual res
  }

  test("testGetEquives") {
    val terms = "(x + y + z),(z + x + y),(j + k + z),(j + k),(k + j)".split(",").map(parser.parseExpression)
    val programs = Programs.empty.addTerm(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, terms))
    val ids = terms.map(t => {
      val (graph, root) = Programs.destructPatternsWithRoots(Seq(t)).head
      programs.hyperGraph.findSubgraph[Int](graph).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
    }).toSeq
    val res = new ObservationalEquivalence(3).getEquives(ActionSearchState(Programs(programs.hyperGraph ++ ids.map(ObservationalEquivalence.createAnchor)), AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    Set(Set(ids take 2), Set(ids.slice(3, 5)), Set(ids(2))) shouldEqual res
  }
}
