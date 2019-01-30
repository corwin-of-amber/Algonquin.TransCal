package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import language.TranscalParser
import org.scalatest.{FunSuite, Matchers}
import structures.immutable.VocabularyHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import syntax.{Identifier, Tree}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LocateAction.LocateMetadata
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

class LetActionTest extends FunSuite with Matchers with LazyLogging {

  test("Bidirectional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat = ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply new ActionSearchState(Programs(new Tree(new Identifier("concat"))), Set.empty)
    newState.rewriteRules.size shouldEqual 3
  }

  test("Directional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat >> ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply new ActionSearchState(Programs(new Tree(new Identifier("concat"))), Set.empty)
    newState.rewriteRules.size shouldEqual 2
  }

  test("Simple let rewrite should match and reconstruct") {
    val letTerm = (new TranscalParser).apply("f ?x >> x + 1")
    val newState = new LetAction(letTerm) apply new ActionSearchState(Programs(new Tree(new Identifier("f"), List(new Tree(new Identifier("3"))))), Set.empty)
    newState.rewriteRules.size shouldEqual 1
    val searchState = newState.rewriteRules.head.apply(new RewriteSearchState(newState.programs.hyperGraph))
    val newEdges = searchState.graph.findEdges(HyperTermIdentifier(new Identifier("+")))
    newEdges.size shouldEqual 1
    Programs(searchState.graph).reconstruct(newEdges.head.target) contains (new TranscalParser).apply("_ -> 3 + 1").subtrees(1) shouldEqual true
  }
}
