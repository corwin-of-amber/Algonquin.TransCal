package synthesis

import java.util.Locale.LanguageRange

import language.{Language, TranscalParser}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.{EmptyMetadata, HyperEdge}
import syntax.Identifier
import synthesis.rewrites.RewriteSearchState

class RewriteRulesDBTest extends PropSpec with Checkers {

  property("rewriteRules manage to rewrite a + b + c to (a + b) + c") {
    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm, Set.empty)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = AssociativeRewriteRulesDB.rewriteRules
    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).nonEmpty))
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

  property("rewriteRules manage to rewrite (?x ≤ ?y) ||> min(x, y) >> id a") {
    val term = new TranscalParser().apply("1 -> min(a, b)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> id a").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm, Set.empty)
    val state = new RewriteSearchState(Programs.destruct(term).addEdges(Set(
      HyperEdge(HyperTermId(100), HyperTermIdentifier(new Identifier("a")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(101), HyperTermIdentifier(new Identifier("b")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(103), HyperTermIdentifier(Language.trueId), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(103), HyperTermIdentifier(new Identifier("≤")), List(HyperTermId(101), HyperTermId(102)), EmptyMetadata)
    )))
    val rules = new SimpleRewriteRulesDB().rewriteRules
    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).nonEmpty))
  }

  property("rewriteRules doesnt rewrite (?x ≤ ?y) ||> min(x, y) >> id(x) by mistake") {
    val term = new TranscalParser().apply("1 -> min(a, b)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> id a").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm, Set.empty)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = new SimpleRewriteRulesDB().rewriteRules
    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).isEmpty))
  }
}
