package synthesis

import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}
import org.scalatest.{FunSpec, FunSuite, Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm

class RewriteRulesDBTest extends FunSuite with Matchers {

  test("rewriteRules manage to rewrite a + b + c to (a + b) + c") {
    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = AssociativeRewriteRulesDB.rewriteRules
    rules.exists(_.apply(state).graph.findSubgraph[Int](pattern).nonEmpty) shouldEqual true
  }

  //  property("rewriteRules manage to rewrite ?x / false >> id x") {
  //    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
  //    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
  //    val pattern = Programs.destructPattern(patternTerm, Set.empty)
  //    val state = new RewriteSearchState(Programs.destruct(term))
  //    val rules = AssociativeRewriteRulesDB.rewriteRules
  //    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).nonEmpty))
  //  }
  //
  //  property("rewriteRules doesnt rewrite false in ?x / false >> id x") {
  //    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
  //    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
  //    val pattern = Programs.destructPattern(patternTerm, Set.empty)
  //    val state = new RewriteSearchState(Programs.destruct(term))
  //    val rules = AssociativeRewriteRulesDB.rewriteRules
  //    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).nonEmpty))
  //  }

  test("rewriteRules manage to rewrite (?x le ?y) ||> min(x, y) >> id x") {
    val term = new TranscalParser().parseExpression("min(a, b) |||| ((a <= b) ||| true)")
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(r => {
      val newProgs = synthesis.Programs(r(state).graph)
      val aRoot = newProgs.hyperGraph.findEdges(HyperTermIdentifier(Identifier("a"))).head.target
      newProgs.reconstruct(aRoot).contains(new TranscalParser().parseExpression("min(a, b)"))
    }) shouldEqual true
  }

  test("rewriteRules doesnt rewrite (?x le ?y) ||> min(x, y) >> id(x) by mistake") {
    val term = new TranscalParser().apply("1 -> min(a, b)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> id a").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(_.apply(state).graph.findSubgraph[Int](pattern).isEmpty) shouldEqual true
  }

  test("rule to a single hole works") {
    val term = new TranscalParser().parseExpression("l ++ ⟨⟩")
    val patternTerm = new TranscalParser().parseExpression("l")
    val (pattern, patternRoot) = Programs.destructPatternsWithRoots(Seq(patternTerm)).head
    val (graph, root) = Programs.destructWithRoot(term)
    val anchor = HyperTermIdentifier(Identifier("anchor"))
    val state = new RewriteSearchState(graph + HyperEdge(root, anchor, Seq.empty, NonConstructableMetadata))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(r => {
      val newState = r.apply(state)
      val id = patternRoot.asInstanceOf[ReferenceTerm[HyperTermId]].id
      newState.graph.findSubgraph[Int](pattern).head._1(id) == newState.graph.findEdges(anchor).head.target
    }) shouldEqual true
  }

  test("And associativity") {
    val rules = AssociativeRewriteRulesDB.rewriteRules
    def validate(term: AnnotatedTree, patternTerm: AnnotatedTree) = {
      val (pattern, patternRoot) = Programs.destructPatternsWithRoots(Seq(patternTerm)).head
      val (graph, root) = Programs.destructWithRoot(term)
      val anchor = HyperTermIdentifier(Identifier("anchor"))
      val state = new RewriteSearchState(graph + HyperEdge(root, anchor, Seq.empty, NonConstructableMetadata))
      rules.exists(r => {
        val newState = r.apply(state)
        val id = patternRoot.asInstanceOf[ReferenceTerm[HyperTermId]].id
        val findRes = newState.graph.findSubgraph[Int](pattern)
        findRes.nonEmpty && findRes.head._1(id) == newState.graph.findEdges(anchor).head.target
      }) shouldEqual true
    }

    val term1 = new TranscalParser().parseExpression("((x ∧ y) ∧ z)")
    val term2 = new TranscalParser().parseExpression("(x ∧ (y ∧ z))")
    validate(term1, term2)
    validate(term2, term1)
  }
}
