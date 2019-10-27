package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, Programs, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{AnnotatedTree, Language, TranscalParser}

class ObservationalEquivalenceTest extends FunSuite with Matchers {
  val parser = new TranscalParser
  val normalRules = SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules

  test("testFromTerms") {
    val eqiuivalentTerms = Seq(
      "x + y",
      "y + x",
      s"x :: y :: ${Language.nilId.literal}",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal})",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal}) ++ ${Language.nilId.literal}"
    ).map(parser.parseExpression)
    val res = new ObservationalEquivalence(3).fromTerms(eqiuivalentTerms, normalRules)
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
    Set(Set(ids take 2: _*), Set(ids.slice(3, 5): _*), Set(ids(2))) shouldEqual res
  }

  test("test all nodes are present in original anchors") {
    val graph = Programs.empty
      .addTerm(parser.parseExpression("x + y + z"))
      .addTerm(parser.parseExpression("x :: l ++ l2"))
      .addTerm(parser.parseExpression("x :: l2"))
      .addTerm(parser.parseExpression("1 + z"))
    val patternRoots = Programs.destructPatternsWithRoots(Seq("?x + ?y + ?z", "?x :: ?l").map(parser.parseExpression))
    val ids = patternRoots.flatMap({case (pattern, root) =>
      graph.hyperGraph.findSubgraph[Int](pattern).map(_._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id))
    })
    val updatedState = ActionSearchState(Programs(graph.hyperGraph ++
      ids.map(ObservationalEquivalence.createAnchor)), normalRules)
    val equives = new ObservationalEquivalence().getEquives(updatedState)
    equives.flatten shouldEqual ids.toSet
  }

  test("test all nodes are present in original graph (auto add anchors)") {
    val graph = Programs.empty
      .addTerm(parser.parseExpression("x + y + z"))
      .addTerm(parser.parseExpression("x :: l ++ l2"))
      .addTerm(parser.parseExpression("x :: l2"))
      .addTerm(parser.parseExpression("1 + z"))
    val updatedState = ActionSearchState(Programs(graph.hyperGraph), normalRules)
    val equives = new ObservationalEquivalence().getEquives(updatedState)
    equives.flatten shouldEqual updatedState.programs.hyperGraph.nodes
    val mutableEqs = new ObservationalEquivalence().getEquivesFromRewriteState(new RewriteSearchState(updatedState.programs.hyperGraph), normalRules)
    mutableEqs._2.flatten shouldEqual updatedState.programs.hyperGraph.nodes
  }
}
