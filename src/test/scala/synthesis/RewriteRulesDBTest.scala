package synthesis

import transcallang.{Language, TranscalParser}
import org.scalatest.{FunSpec, FunSuite, Matchers, PropSpec}
import org.scalatest.prop.Checkers
import structures.{EmptyMetadata, HyperEdge}
import syntax.Identifier
import synthesis.rewrites.RewriteSearchState

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
    val term = new TranscalParser().apply("1 -> min(a, b)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> id a").subtrees(1)
    val resultPattern = Programs.destructPattern(patternTerm)
    val state = new RewriteSearchState(Programs.destruct(term).addEdges(Set(
      HyperEdge(HyperTermId(100), HyperTermIdentifier(new Identifier("a")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(101), HyperTermIdentifier(new Identifier("b")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(103), HyperTermIdentifier(Language.trueId), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(103), HyperTermIdentifier(new Identifier("≤")), List(HyperTermId(100), HyperTermId(101)), EmptyMetadata)
    )))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(r => r.apply(state).graph.findSubgraph[Int](resultPattern).nonEmpty) shouldEqual true
  }

  test("rewriteRules doesnt rewrite (?x le ?y) ||> min(x, y) >> id(x) by mistake") {
    val term = new TranscalParser().apply("1 -> min(a, b)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> id a").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(_.apply(state).graph.findSubgraph[Int](pattern).isEmpty) shouldEqual true
  }

  test("rewriteRules can rewrite correct matches") {
    val term = new TranscalParser().apply("1 -> true match (true ⇒ hello / false => world)").subtrees(1)
    val (graph, root) = Programs.destructWithRoot(term)
    val state = new RewriteSearchState(graph)
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(_.apply(state).graph.exists(e => e.target == root && e.edgeType.identifier.literal.toString == "hello")) shouldEqual true
  }
}
